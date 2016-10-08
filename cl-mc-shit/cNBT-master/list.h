#ifndef LIST_H
#define LIST_H

#include <stddef.h>

/*
 * Represents a single entry in the list. This must be embedded in your linked
 * structure.
 */
struct list_head {
    struct list_head *blink, /* back  link */
                     *flink; /* front link */
};

/* The first element is a sentinel. Don't access it. */
#define INIT_LIST_HEAD(head) (head)->flink = (head)->blink = (head)

/* Adds a new element to the beginning of a list. Returns the head of the list
 * so that calls may be chained. */
static inline struct list_head* list_add_head(struct list_head* restrict new_element,
                                              struct list_head* restrict head)
{
    new_element->flink = head->flink;
    new_element->blink = head;

    new_element->flink->blink = new_element;
    new_element->blink->flink = new_element;

    return head;
}

/* Adds a new element to the end of a list. Returns the head of the list so that
 * calls may be chained. */
static inline struct list_head* list_add_tail(struct list_head* restrict new_element,
                                              struct list_head* restrict head)
{
    new_element->flink = head;
    new_element->blink = head->blink;

    new_element->flink->blink = new_element;
    new_element->blink->flink = new_element;

    return head;
}

/* Deletes an element from a list. NOTE: This does not free any memory. */
static inline void list_del(struct list_head* loc)
{
    loc->flink->blink = loc->blink;
    loc->blink->flink = loc->flink;

    loc->flink = NULL;
    loc->blink = NULL;
}

/* Tests if the list is empty */
#define list_empty(head) ((head)->flink == (head))

/* Gets a pointer to the overall structure from the list member */
#define list_entry(ptr, type, member) \
    ((type*)((char*)(ptr) - offsetof(type, member)))

/*
 * Iterates over all the elements forward. If you modify the list (such as by
 * deleting an element), you should use list_for_each_safe instead.
 */
#define list_for_each(pos, head) \
    for((pos) = (head)->flink;   \
        (pos) != (head);         \
        (pos) = (pos)->flink)

/* The same as list_for_each, except it traverses the list backwards. */
#define list_for_each_reverse(pos, head) \
    for((pos) = (head)->blink;           \
        (pos) != (head);                 \
        (pos) = (pos)->blink)

/*
 * Iterates over a list, where `pos' represents the current element, `n'
 * represents temporary storage for the next element, and `head' is the start of
 * the list.
 *
 * As opposed to list_for_each, it is safe to remove `pos' from the list.
 */
#define list_for_each_safe(pos, n, head)           \
    for((pos) = (head)->flink, (n) = (pos)->flink; \
        (pos) != (head);                           \
        (pos) = (n), (n) = (pos)->flink)

/* The same as list_for_each_safe, except it traverses the list backwards. */
#define list_for_each_reverse_safe(pos, p, head)   \
    for((pos) = (head)->blink, (p) = (pos)->blink; \
        (pos) != (head);                           \
        (pos) = (p), (p) = (pos)->blink)

/*
 * Returns the length of a list. WARNING: Unlike every other function, this runs
 * in O(n). Avoid using it as much as possible, as you will have to walk the
 * whole list.
 */
static inline size_t list_length(const struct list_head* head)
{
    const struct list_head* cursor;
    size_t accum = 0;

    list_for_each(cursor, head)
        accum++;

    return accum;
}

#endif
