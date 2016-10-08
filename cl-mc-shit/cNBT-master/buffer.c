/*
* -----------------------------------------------------------------------------
* "THE BEER-WARE LICENSE" (Revision 42):
* Lukas Niederbremer <webmaster@flippeh.de> and Clark Gaebel <cg.wowus.cg@gmail.com>
* wrote this file. As long as you retain this notice you can do whatever you
* want with this stuff. If we meet some day, and you think this stuff is worth
* it, you can buy us a beer in return.
* -----------------------------------------------------------------------------
*/
#include "buffer.h"

#include <assert.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

#ifdef __GNUC__
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(  (x), 0)
#else
#define likely(x)   (x)
#define unlikely(x) (x)
#endif

static int lazy_init(struct buffer* b)
{
    assert(b->data == NULL);

    size_t cap = 1024;

    *b = (struct buffer) {
        .data = malloc(cap),
        .len  = 0,
        .cap  = cap
    };

    if(unlikely(b->data == NULL))
        return 1;

    return 0;
}

void buffer_free(struct buffer* b)
{
    assert(b);

    free(b->data);

    b->data = NULL;
    b->len = 0;
    b->cap = 0;
}

int buffer_reserve(struct buffer* b, size_t reserved_amount)
{
    assert(b);

    if(unlikely(b->data == NULL) &&
       unlikely(lazy_init(b)))
        return 1;

    if(likely(b->cap >= reserved_amount))
        return 0;

    while(b->cap < reserved_amount)
        b->cap *= 2;

    unsigned char* temp = realloc(b->data, b->cap);

    if(unlikely(temp == NULL))
        return buffer_free(b), 1;

    b->data = temp;

    return 0;
}

int buffer_append(struct buffer* b, const void* data, size_t n)
{
    assert(b);

    if(unlikely(b->data == NULL) &&
       unlikely(lazy_init(b)))
        return 1;

    if(unlikely(buffer_reserve(b, b->len + n)))
        return 1;

    memcpy(b->data + b->len, data, n);
    b->len += n;

    return 0;
}

