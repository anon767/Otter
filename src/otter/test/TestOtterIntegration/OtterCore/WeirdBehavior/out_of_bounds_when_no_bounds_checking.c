/*  This test leads to an ImmutableArray.Out_of_bounds exception that happens when there is an overflow and
    --noboundsChecking is given. Strictly speaking, this correctly an error, and --noboundsChecking should never be
    used unless there is no overflow in the program.

    We should consider removing --noboundsChecking instead of softening this exception, since, after an overflow,
    the behavior of the program is undefined, and any subsequently reported errors becomes unreliable.
 */

#pragma no_bounds_checking

int main(void) {
    int x[1] = { 0 };
    return x[1];
}
