/*
 * An example showing how the new BackOtter framework works
 *
 * It begins by executing g. It finds a failing path (n == 0), and is paused.
 * Next, f is executed. It first follows the path (m == 1), and runs g(1).
 *      i. Based on g's execution tree, it first tries to execute g(1) following the failing path.
 *      ii. The failing path is infeasible. It then executes g(1) based on the calling context,
 *          and updates g's execution tree.
 *          g's execution tree will now have two paths, (n == 0) and (n == 1), the latter coming from the
 *          recent execution of calling g(1).
 * It goes back to f, and executes the other path (m != 0), which leads to the call g(m).
 * It again tries to run g(m) following the failing path (n == 0). That's again infeasible.
 * Now, instead of blindly running g again, since we know that (n == 1) does not lead of failure, it
 * first tries the path (n == 2). And it hits the failure.
 *
 * This example shows that:
 * 1. We maintain an execution tree for each symbolic execution of a function. This gives information of
 *    i. Existing failing paths;
 *    ii. Covered non-failing paths.
 * 2. If it spends time on executing paths that do not yield failures, these paths are stored in the
 *    execution tree, and it avoids re-executing them later.
 */

#include "otter.h"

void g(int n) {
    if (n == 0) {
        __FAILURE();
    } else if (n == 1) {
        // OK
    } else if (n == 2) {
        __FAILURE();
    }
}

void f(int m) {
    if (m == 1) {
        g(1);
    } else if (m != 0) {
        g(m);
    }
}

void main() {
    int x; __SYMBOLIC(&x);
    f(x);
}
