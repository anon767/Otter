/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=(any number) test/TestBackOtter/sglib.c
 * (This test becomes not working after functions are made call-by-reference.
 *  Likely the Conditional Exception stuff is not working here.)
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
    if (*elem == 0) __ASSERT(0);
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
