/*
 * ./otter.pl --dobackotter 02.sglib.c
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
 */

#pragma expect_abandoned(target_reached, g != 0)
#pragma expect_return(g == 0)
#pragma no_other_abandoned
#pragma no_other_return
#pragma no_other_exit

int g;

void __FAILURE(void) { }

struct doubly_linkedlist {
    int data;
    struct doubly_linkedlist* prev;
    struct doubly_linkedlist* next;
};

void sglib___dl_list_create_singleton(struct doubly_linkedlist** list, struct doubly_linkedlist** elem) {
  (*list) = (*elem);
  (*list)->next = (*list)->prev = 0;
}

void sglib_dl_list_add_after(struct doubly_linkedlist** place, struct doubly_linkedlist** elem) {
  if ((*place) == 0) {
    sglib___dl_list_create_singleton(place, elem);
  } else {
    if (*elem == 0) __FAILURE();
    (*elem)->next = (*place)->next;
    (*elem)->prev = (*place);
    (*place)->next = (*elem);
    if ((*elem)->next != 0) (*elem)->next->prev = (*elem);
  }
}

void sglib_dl_list_concat(struct doubly_linkedlist** first, struct doubly_linkedlist** second) {
  if ((*first)==0) {
    (*first) = (*second);
  } else {
    struct doubly_linkedlist*_dlp_;
    for(_dlp_ = (*first); _dlp_->next!=0; _dlp_=_dlp_->next) ;
    sglib_dl_list_add_after(&_dlp_, second);
  }
}

void main() {
    struct doubly_linkedlist* first;
    struct doubly_linkedlist* second;

    __SYMBOLIC(&g);

    first = malloc(sizeof(struct doubly_linkedlist));
    first->prev = 0;
    first->next = 0;

    first = g?first:0;
    second = 0;
    sglib_dl_list_concat(&first, &second);
}
