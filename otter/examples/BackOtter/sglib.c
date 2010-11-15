/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=1 --no-exceptions-as-failures -Ilibc test/TestBackOtter/sglib.c
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF ((unsigned int )*elem == (unsigned int )((struct doubly_linkedlist *)0)
 *                    ): true
 *          Decision: IF ((unsigned int )*place == (unsigned int )((struct doubly_linkedlist *)0)
 *                    ): false
 *          Decision: sglib_dl_list_add_after: void (struct doubly_linkedlist **place ,
 *                                                    struct doubly_linkedlist **elem )
 *          Decision: IF ((unsigned int )_dlp_->next != (unsigned int )((struct doubly_linkedlist *)0)
 *                    ): false
 *          Decision: IF ((unsigned int )*first == (unsigned int )((struct doubly_linkedlist *)0)
 *                    ): false
 *          Decision: sglib_dl_list_concat: void (struct doubly_linkedlist **first ,
 *                                                 struct doubly_linkedlist **second )
 *
 * -----------------------------------------
 * TODO: The SE gets stuck at reasoning about several symbolic pointers. Currently, the above desired result can be
 *       obtained by manually stopping (ctrl-c) stuck paths. We want
 *       1. Automatic stopping a freezing path;
 *       2. Better Stp Caching technique.
 */
#include "otter.h"

#define NULL 0

struct doubly_linkedlist {
    int data;
    struct doubly_linkedlist* prev;
    struct doubly_linkedlist* next;
};

#define DL  struct doubly_linkedlist

void sglib___dl_list_create_singleton(DL** list, DL** elem) {
  (*list) = (*elem);
  (*list)->next = (*list)->prev = NULL;
}

void sglib_dl_list_add_after(DL** place, DL** elem) {
  if ((*place) == NULL) {
    sglib___dl_list_create_singleton(place, elem);
  } else {
    if (*elem == 0) __FAILURE();
    (*elem)->next = (*place)->next;
    (*elem)->prev = (*place);
    (*place)->next = (*elem);
    if ((*elem)->next != NULL) (*elem)->next->prev = (*elem);
  }
}

void sglib_dl_list_concat(DL** first, DL** second) {
  if ((*first)==NULL) {
    (*first) = (*second);
  } else {
    DL*_dlp_;
    for(_dlp_ = (*first); _dlp_->next!=NULL; _dlp_=_dlp_->next) ;
    sglib_dl_list_add_after(&_dlp_, second);
  }
}

void main() {
    DL* first;
    DL* second;

    first = malloc(sizeof(DL));
    first->prev = 0;
    first->next = 0;
    second = 0;
    sglib_dl_list_concat(&first, &second);
}
