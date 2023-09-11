/* Symbolic branching is confusing when there are also multiple processes.
   Here's why this test should have 8 outputs:
   1. At first, there is only one job containing only one process. Let's call it
   parent0.
   2. At fork, a child process gets created, child0.
   3. Either parent0 or child0 executes the if first. WLOG, say it's parent0.
   4. The if causes parent0 and child0 to be duplicated as parent1 and child1
   (where the 0s take one branch and the 1s take the other).
   5. When child0 executes the if, parent0 and child0 are again duplicated, this
   time into parent2 and child2.
   6. When child1 executes the if, parent1 and child1 are duplicated into
   parent3 and child3
   So, in the end, there are 4 path conditions (all combinations of true and
   false for the parent's x and the child's x), each with parent and child
   processes.

   I suspect this test may be a bit fragile, though, meaning that it relies on
   some details of MultiOtter's implementation that may be subject to change.
   For example, here's an alternative way this program could be executed:
   1. to 4. Same as above.
   5. parent0 finishes executing.
   6. parent1 finishes executing.
   7. child0 executes the if, duplicating itself into child2. **But parent0
   doesn't exist anymore, so it does not get duplicated into parent2.**
   8. Same for child1, creating child3, but not parent3.
   So, in this execution, there are 6 paths: path conditions 0 and 1 have parent
   and child processes, but path conditions 2 and 3 have only child processes.

   We should think about whether this is acceptable behavior, or whether should
   also duplicate *completed* processes when we symbolically branch.
*/

#pragma expect_return(__return_code__ == 0);
#pragma expect_return(__return_code__ == 0);
#pragma expect_return(__return_code__ == 0);
#pragma expect_return(__return_code__ == 0);
#pragma expect_return(__return_code__ == 1);
#pragma expect_return(__return_code__ == 1);
#pragma expect_return(__return_code__ == 1);
#pragma expect_return(__return_code__ == 1);
#pragma no_other_results

int main() {
    __otter_multi_fork();
    int x;
    __SYMBOLIC(&x);
    if (x);
    return __otter_multi_get_pid();
}
