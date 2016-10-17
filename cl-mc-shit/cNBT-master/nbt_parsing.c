/*
 * -----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * Lukas Niederbremer <webmaster@flippeh.de> and Clark Gaebel <cg.wowus.cg@gmail.com>
 * wrote this file. As long as you retain this notice you can do whatever you
 * want with this stuff. If we meet some day, and you think this stuff is worth
 * it, you can buy us a beer in return.
 * -----------------------------------------------------------------------------
 */
#include "nbt.h"

#include "buffer.h"
#include "list.h"

#include <assert.h>
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

/* are we running on a little-endian system? */
static int little_endian()
{
    uint16_t t = 0x0001;
    char c[2];
    memcpy(c, &t, sizeof t);
    return c[0];
}

static void* swap_bytes(void* s, size_t len)
{
    for(char* b = s,
            * e = b + len - 1;
        b < e;
        b++, e--)
    {
        char t = *b;

        *b = *e;
        *e = t;
    }

    return s;
}

/* big endian to native endian. works in-place */
static void* be2ne(void* s, size_t len)
{
    return little_endian() ? swap_bytes(s, len) : s;
}

/* native endian to big endian. works the exact same as its inverse */
#define ne2be be2ne

/* A special form of memcpy which copies `n' bytes into `dest', then returns
 * `src' + n.
 */
static const void* memscan(void* dest, const void* src, size_t n)
{
    memcpy(dest, src, n);
    return (const char*)src + n;
}

/* Does a memscan, then goes from big endian to native endian on the
 * destination.
 */
static const void* swapped_memscan(void* dest, const void* src, size_t n)
{
    const void* ret = memscan(dest, src, n);
    return be2ne(dest, n), ret;
}

#define CHECKED_MALLOC(var, n, on_error) do { \
    if((var = malloc(n)) == NULL)             \
    {                                         \
        errno = NBT_EMEM;                     \
        on_error;                             \
    }                                         \
} while(0)

#define CHECKED_APPEND(b, ptr, len) do { \
    if(buffer_append((b), (ptr), (len))) \
        return NBT_EMEM;                 \
} while(0)

/* Parses a tag, given a name (may be NULL) and a type. Fills in the payload. */
static nbt_node* parse_unnamed_tag(nbt_type type, char* name, const char** memory, size_t* length);

/*
 * Reads some bytes from the memory stream. This macro will read `n'
 * bytes into `dest', call either memscan or swapped_memscan depending on
 * `scanner', then fix the length. If anything funky goes down, `on_failure'
 * will be executed.
 */
#define READ_GENERIC(dest, n, scanner, on_failure) do { \
    if(*length < (n)) { on_failure; }                   \
    *memory = scanner((dest), *memory, (n));            \
    *length -= (n);                                     \
} while(0)

/* printfs into the end of a buffer. Note: no null-termination! */
static void bprintf(struct buffer* b, const char* restrict format, ...)
{
    va_list args;
    int siz;

    va_start(args, format);
    siz = vsnprintf(NULL, 0, format, args);
    va_end(args);

    buffer_reserve(b, b->len + siz + 1);

    va_start(args, format);
    vsnprintf((char*)(b->data + b->len), siz + 1, format, args);
    va_end(args);

    b->len += siz; // remember - no null terminator!
}

/*
 * Reads a string from memory, moving the pointer and updating the length
 * appropriately. Returns NULL on failure.
 */
static char* read_string(const char** memory, size_t* length)
{
    int16_t string_length;
    char* ret = NULL;

    READ_GENERIC(&string_length, sizeof string_length, swapped_memscan, goto parse_error);

    if(string_length < 0)               goto parse_error;
    if(*length < (size_t)string_length) goto parse_error;

    CHECKED_MALLOC(ret, string_length + 1, goto parse_error);

    READ_GENERIC(ret, (size_t)string_length, memscan, goto parse_error);

    ret[string_length] = '\0'; /* don't forget to NULL-terminate ;) */
    return ret;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;

    free(ret);
    return NULL;
}

static nbt_node* parse_named_tag(const char **memory, size_t *length)
{
  char* name = NULL;

  uint8_t type;
  READ_GENERIC(&type, sizeof type, memscan, goto parse_error);

  name = read_string(memory, length);

  nbt_node* ret = parse_unnamed_tag((nbt_type)type, name, memory, length);
  if(ret == NULL) goto parse_error;

  return ret;

parse_error:
  if(errno == NBT_OK)
    errno = NBT_ERR;

  free(name);
  return NULL;
}

static struct nbt_byte_array read_byte_array(const char** memory, size_t* length)
{
    struct nbt_byte_array ret;
    ret.data = NULL;

    READ_GENERIC(&ret.length, sizeof ret.length, swapped_memscan, goto parse_error);

    if(ret.length < 0) goto parse_error;

    CHECKED_MALLOC(ret.data, ret.length, goto parse_error);

    READ_GENERIC(ret.data, (size_t)ret.length, memscan, goto parse_error);

    return ret;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;

    free(ret.data);
    ret.data = NULL;
    return ret;
}

static struct nbt_int_array read_int_array(const char** memory, size_t* length)
{
    struct nbt_int_array ret;
    ret.data = NULL;

    READ_GENERIC(&ret.length, sizeof ret.length, swapped_memscan, goto parse_error);

    if(ret.length < 0) goto parse_error;

    CHECKED_MALLOC(ret.data, ret.length * sizeof(int32_t), goto parse_error);

    READ_GENERIC(ret.data, (size_t)ret.length * sizeof(int32_t), memscan, goto parse_error);


    // Byteswap the whole array.
    for(int32_t i = 0; i < ret.length; i++)
        be2ne(ret.data + i, sizeof(int32_t));

    return ret;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;

    free(ret.data);
    ret.data = NULL;
    return ret;
}

/*
 * Is the list all one type? If yes, return the type. Otherwise, return
 * TAG_INVALID
 */
static nbt_type list_is_homogenous(const struct nbt_list* list)
{
    nbt_type type = TAG_INVALID;

    const struct list_head* pos;
    list_for_each(pos, &list->entry)
    {
        const struct nbt_list* cur = list_entry(pos, const struct nbt_list, entry);

        assert(cur->data);
        assert(cur->data->type != TAG_INVALID);

        if(cur->data->type == TAG_INVALID)
            return TAG_INVALID;

        /* if we're the first type, just set it to our current type */
        if(type == TAG_INVALID) type = cur->data->type;

        if(type != cur->data->type)
            return TAG_INVALID;
    }

    /* if the list was empty, use the sentinel type */
    if(type == TAG_INVALID && list->data != NULL)
        type = list->data->type;

    return type;
}

static struct nbt_list* read_list(const char** memory, size_t* length)
{
    uint8_t type;
    int32_t elems;
    struct nbt_list* ret;

    CHECKED_MALLOC(ret, sizeof *ret, goto parse_error);

    /* we allocate the data pointer to store the type of the list in the first
     * sentinel element */
    CHECKED_MALLOC(ret->data, sizeof *ret->data, goto parse_error);

    INIT_LIST_HEAD(&ret->entry);

    READ_GENERIC(&type, sizeof type, swapped_memscan, goto parse_error);
    READ_GENERIC(&elems, sizeof elems, swapped_memscan, goto parse_error);

    ret->data->type = type == TAG_INVALID ? TAG_COMPOUND : (nbt_type)type;

    for(int32_t i = 0; i < elems; i++)
    {
        struct nbt_list* new;

        CHECKED_MALLOC(new, sizeof *new, goto parse_error);

        new->data = parse_unnamed_tag((nbt_type)type, NULL, memory, length);

        if(new->data == NULL)
        {
            free(new);
            goto parse_error;
        }

        list_add_tail(&new->entry, &ret->entry);
    }

    return ret;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;

    nbt_free_list(ret);
    return NULL;
}

static struct nbt_list* read_compound(const char** memory, size_t* length)
{
    struct nbt_list* ret;

    CHECKED_MALLOC(ret, sizeof *ret, goto parse_error);

    ret->data = NULL;
    INIT_LIST_HEAD(&ret->entry);

    for(;;)
    {
        uint8_t type;
        char* name = NULL;
        struct nbt_list* new_entry;

        READ_GENERIC(&type, sizeof type, swapped_memscan, goto parse_error);

        if(type == 0) break; /* TAG_END == 0. We've hit the end of the list when type == TAG_END. */

        name = read_string(memory, length);
        if(name == NULL) goto parse_error;

        CHECKED_MALLOC(new_entry, sizeof *new_entry,
            free(name);
            goto parse_error;
        );

        new_entry->data = parse_unnamed_tag((nbt_type)type, name, memory, length);

        if(new_entry->data == NULL)
        {
            free(new_entry);
            free(name);
            goto parse_error;
        }

        list_add_tail(&new_entry->entry, &ret->entry);
    }

    return ret;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;
    nbt_free_list(ret);

    return NULL;
}

/*
 * Parses a tag, given a name (may be NULL) and a type. Fills in the payload.
 */
static nbt_node* parse_unnamed_tag(nbt_type type, char* name, const char** memory, size_t* length)
{
    nbt_node* node;

    CHECKED_MALLOC(node, sizeof *node, goto parse_error);

    node->type = type;
    node->name = name;

#define COPY_INTO_PAYLOAD(payload_name) \
    READ_GENERIC(&node->payload.payload_name, sizeof node->payload.payload_name, swapped_memscan, goto parse_error);

    switch(type)
    {
    case TAG_BYTE:
        COPY_INTO_PAYLOAD(tag_byte);
        break;
    case TAG_SHORT:
        COPY_INTO_PAYLOAD(tag_short);
        break;
    case TAG_INT:
        COPY_INTO_PAYLOAD(tag_int);
        break;
    case TAG_LONG:
        COPY_INTO_PAYLOAD(tag_long);
        break;
    case TAG_FLOAT:
        COPY_INTO_PAYLOAD(tag_float);
        break;
    case TAG_DOUBLE:
        COPY_INTO_PAYLOAD(tag_double);
        break;
    case TAG_BYTE_ARRAY:
        node->payload.tag_byte_array = read_byte_array(memory, length);
        break;
    case TAG_INT_ARRAY:
        node->payload.tag_int_array = read_int_array(memory, length);
        break;
    case TAG_STRING:
        node->payload.tag_string = read_string(memory, length);
        break;
    case TAG_LIST:
        node->payload.tag_list = read_list(memory, length);
        break;
    case TAG_COMPOUND:
        node->payload.tag_compound = read_compound(memory, length);
        break;

    default:
        goto parse_error; /* Unknown node or TAG_END. Either way, we shouldn't be parsing this. */
    }

#undef COPY_INTO_PAYLOAD

    if(errno != NBT_OK) goto parse_error;

    return node;

parse_error:
    if(errno == NBT_OK)
        errno = NBT_ERR;

    free(node);
    return NULL;
}

nbt_node* nbt_parse(const void* mem, size_t len)
{
    errno = NBT_OK;

    const char** memory = (const char**)&mem;
    size_t* length = &len;

    return parse_named_tag(memory, length);
}

/* spaces, not tabs ;) */
static void indent(struct buffer* b, size_t amount)
{
    size_t spaces = amount * 4; /* 4 spaces per indent */

    char temp[spaces + 1];

    for(size_t i = 0; i < spaces; ++i)
        temp[i] = ' ';
    temp[spaces] = '\0';

    bprintf(b, "%s", temp);
}

static nbt_status __nbt_dump_ascii(const nbt_node*, struct buffer*, size_t ident);

/* prints the node's name, or (null) if it has none. */
#define SAFE_NAME(node) ((node)->name ? (node)->name : "<null>")

static void dump_byte_array(const struct nbt_byte_array ba, struct buffer* b)
{
    assert(ba.length >= 0);

    bprintf(b, "[ ");
    for(int32_t i = 0; i < ba.length; ++i)
        bprintf(b, "%u ", +ba.data[i]);
    bprintf(b, "]");
}

static void dump_int_array(const struct nbt_int_array ia, struct buffer* b)
{
    assert(ia.length >= 0);

    bprintf(b, "[ ");
    for(int32_t i = 0; i < ia.length; ++i)
        bprintf(b, "%u ", +ia.data[i]);
    bprintf(b, "]");
}

nbt_list* thefuck(const struct list_head *pos){
struct nbt_list* entry = list_entry(pos, const struct nbt_list, entry);
return entry;
}

static nbt_status dump_list_contents_ascii(const struct nbt_list* list, struct buffer* b, size_t ident)
{
    const struct list_head* pos;

for(pos = (&list->entry)->flink; pos != &list->entry; pos = pos->flink)
 //   list_for_each(pos, &list->entry)
    {
        const struct nbt_list* entry = list_entry(pos, const struct nbt_list, entry);
        nbt_status err;

        if((err = __nbt_dump_ascii(entry->data, b, ident)) != NBT_OK)
            return err;
    }

    return NBT_OK;
}

static nbt_status __nbt_dump_ascii(const nbt_node* tree, struct buffer* b, size_t ident)
{
    if(tree == NULL) return NBT_OK;

    indent(b, ident);

    if(tree->type == TAG_BYTE)
        bprintf(b, "TAG_Byte(\"%s\"): %i\n", SAFE_NAME(tree), (int)tree->payload.tag_byte);
    else if(tree->type == TAG_SHORT)
        bprintf(b, "TAG_Short(\"%s\"): %i\n", SAFE_NAME(tree), (int)tree->payload.tag_short);
    else if(tree->type == TAG_INT)
        bprintf(b, "TAG_Int(\"%s\"): %i\n", SAFE_NAME(tree), (int)tree->payload.tag_int);
    else if(tree->type == TAG_LONG)
        bprintf(b, "TAG_Long(\"%s\"): %" PRIi64 "\n", SAFE_NAME(tree), tree->payload.tag_long);
    else if(tree->type == TAG_FLOAT)
        bprintf(b, "TAG_Float(\"%s\"): %f\n", SAFE_NAME(tree), (double)tree->payload.tag_float);
    else if(tree->type == TAG_DOUBLE)
        bprintf(b, "TAG_Double(\"%s\"): %f\n", SAFE_NAME(tree), tree->payload.tag_double);
    else if(tree->type == TAG_BYTE_ARRAY)
    {
        bprintf(b, "TAG_Byte_Array(\"%s\"): ", SAFE_NAME(tree));
        dump_byte_array(tree->payload.tag_byte_array, b);
        bprintf(b, "\n");
    }
    else if(tree->type == TAG_INT_ARRAY)
    {
        bprintf(b, "Tag_Int_Array(\"%s\"): ", SAFE_NAME(tree));
        dump_int_array(tree->payload.tag_int_array, b);
        bprintf(b, "\n");
    }
    else if(tree->type == TAG_STRING)
    {
        if(tree->payload.tag_string == NULL)
            return NBT_ERR;

        bprintf(b, "TAG_String(\"%s\"): %s\n", SAFE_NAME(tree), tree->payload.tag_string);
    }
    else if(tree->type == TAG_LIST)
    {
        bprintf(b, "TAG_List(\"%s\") [%s]\n", SAFE_NAME(tree), nbt_type_to_string(tree->payload.tag_list->data->type));
        indent(b, ident);
        bprintf(b, "{\n");

        nbt_status err = dump_list_contents_ascii(tree->payload.tag_list, b, ident + 1);

        indent(b, ident);
        bprintf(b, "}\n");

        if(err != NBT_OK)
            return err;
    }
    else if(tree->type == TAG_COMPOUND)
    {
        bprintf(b, "TAG_Compound(\"%s\")\n", SAFE_NAME(tree));
        indent(b, ident);
        bprintf(b, "{\n");

        nbt_status err = dump_list_contents_ascii(tree->payload.tag_compound, b, ident + 1);

        indent(b, ident);
        bprintf(b, "}\n");

        if(err != NBT_OK)
            return err;
    }

    else
        return NBT_ERR;

    return NBT_OK;
}

char* nbt_dump_ascii(const nbt_node* tree)
{
    errno = NBT_OK;

    assert(tree);

    struct buffer b = BUFFER_INIT;

    if((errno = __nbt_dump_ascii(tree, &b, 0)) != NBT_OK) goto OOM;
    if(         buffer_reserve(&b, b.len + 1))            goto OOM;

    b.data[b.len] = '\0'; /* null-terminate that biatch, since bprintf doesn't
                             do that for us. */

    return (char*)b.data;

OOM:
    if(errno != NBT_OK)
        errno = NBT_EMEM;

    buffer_free(&b);
    return NULL;
}

static nbt_status dump_byte_array_binary(const struct nbt_byte_array ba, struct buffer* b)
{
    int32_t dumped_length = ba.length;

    ne2be(&dumped_length, sizeof dumped_length);

    CHECKED_APPEND(b, &dumped_length, sizeof dumped_length);

    if(ba.length) assert(ba.data);

    CHECKED_APPEND(b, ba.data, ba.length);

    return NBT_OK;
}

static nbt_status dump_int_array_binary(const struct nbt_int_array ia, struct buffer* b)
{
    int32_t dumped_length = ia.length;

    ne2be(&dumped_length, sizeof dumped_length);

    CHECKED_APPEND(b, &dumped_length, sizeof dumped_length);

    if(ia.length) assert(ia.data);

    for(int32_t i = 0; i < ia.length; i++)
    {
        int32_t swappedElem = ia.data[i];
        ne2be(&swappedElem, sizeof(swappedElem));
        CHECKED_APPEND(b, &swappedElem, sizeof(swappedElem));
    }

    return NBT_OK;
}

static nbt_status dump_string_binary(const char* name, struct buffer* b)
{
    assert(name);

    size_t len = strlen(name);

    if(len > 32767 /* SHORT_MAX */)
        return NBT_ERR;

    { /* dump the length */
        int16_t dumped_len = (int16_t)len;
        ne2be(&dumped_len, sizeof dumped_len);

        CHECKED_APPEND(b, &dumped_len, sizeof dumped_len);
    }

    CHECKED_APPEND(b, name, len);

    return NBT_OK;
}

static nbt_status __dump_binary(const nbt_node*, bool, struct buffer*);

static nbt_status dump_list_binary(const struct nbt_list* list, struct buffer* b)
{
    nbt_type type = list_is_homogenous(list);

    size_t len = list_length(&list->entry);

    if(len > 2147483647 /* INT_MAX */)
        return NBT_ERR;

    assert(type != TAG_INVALID);

    if(type == TAG_INVALID)
        return NBT_ERR;

    {
        int8_t _type = (int8_t)type;
        ne2be(&_type, sizeof _type); /* unnecessary, but left in to keep similar code looking similar */
        CHECKED_APPEND(b, &_type, sizeof _type);
    }

    {
        int32_t dumped_len = (int32_t)len;
        ne2be(&dumped_len, sizeof dumped_len);
        CHECKED_APPEND(b, &dumped_len, sizeof dumped_len);
    }

    const struct list_head* pos;
    list_for_each(pos, &list->entry)
    {
        const struct nbt_list* entry = list_entry(pos, const struct nbt_list, entry);
        nbt_status ret;

        if((ret = __dump_binary(entry->data, false, b)) != NBT_OK)
            return ret;
    }

    return NBT_OK;
}

static nbt_status dump_compound_binary(const struct nbt_list* list, struct buffer* b)
{
    const struct list_head* pos;
    list_for_each(pos, &list->entry)
    {
        const struct nbt_list* entry = list_entry(pos, const struct nbt_list, entry);
        nbt_status ret;

        if((ret = __dump_binary(entry->data, true, b)) != NBT_OK)
            return ret;
    }

    /* write out TAG_End */
    uint8_t zero = 0;
    CHECKED_APPEND(b, &zero, sizeof zero);

    return NBT_OK;
}

/*
 * @param dump_type   Should we dump the type, or just skip it? We need to skip
 *                    when dumping lists, because the list header already says
 *                    the type.
 */
static nbt_status __dump_binary(const nbt_node* tree, bool dump_type, struct buffer* b)
{
    if(dump_type)
    { /* write out the type */
        int8_t type = (int8_t)tree->type;

        CHECKED_APPEND(b, &type, sizeof type);
    }

    if(tree->name)
    {
        nbt_status err;

        if((err = dump_string_binary(tree->name, b)) != NBT_OK)
            return err;
    }

#define DUMP_NUM(type, x) do {               \
    type temp = x;                           \
    ne2be(&temp, sizeof temp);               \
    CHECKED_APPEND(b, &temp, sizeof temp);   \
} while(0)

    if(tree->type == TAG_BYTE)
        DUMP_NUM(int8_t, tree->payload.tag_byte);
    else if(tree->type == TAG_SHORT)
        DUMP_NUM(int16_t, tree->payload.tag_short);
    else if(tree->type == TAG_INT)
        DUMP_NUM(int32_t, tree->payload.tag_int);
    else if(tree->type == TAG_LONG)
        DUMP_NUM(int64_t, tree->payload.tag_long);
    else if(tree->type == TAG_FLOAT)
        DUMP_NUM(float, tree->payload.tag_float);
    else if(tree->type == TAG_DOUBLE)
        DUMP_NUM(double, tree->payload.tag_double);
    else if(tree->type == TAG_BYTE_ARRAY)
        return dump_byte_array_binary(tree->payload.tag_byte_array, b);
    else if(tree->type == TAG_INT_ARRAY)
        return dump_int_array_binary(tree->payload.tag_int_array, b);
    else if(tree->type == TAG_STRING)
        return dump_string_binary(tree->payload.tag_string, b);
    else if(tree->type == TAG_LIST)
        return dump_list_binary(tree->payload.tag_list, b);
    else if(tree->type == TAG_COMPOUND)
        return dump_compound_binary(tree->payload.tag_compound, b);

    else
        return NBT_ERR;

    return NBT_OK;

#undef DUMP_NUM
}

struct buffer nbt_dump_binary(const nbt_node* tree)
{
    errno = NBT_OK;

    if(tree == NULL) return BUFFER_INIT;

    struct buffer ret = BUFFER_INIT;

    errno = __dump_binary(tree, true, &ret);

    return ret;
}
