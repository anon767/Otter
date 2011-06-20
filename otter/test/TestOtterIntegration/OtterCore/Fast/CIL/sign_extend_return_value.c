/* This weird bug occurs when a function is used before it's defined and without declaring its prototype, and the
 * return value type does not match the destination type.
 *
 * Normally, if the prototype is correctly declared and the return types do not match, CIL will insert an explicit cast
 * which Otter handles correctly.
 *
 * When the prototype isn't declared, the return type is implicitly int. If the actual return type is not an int,
 * compilers such as GCC or Clang will report an error.
 *
 * Unless strict consistency checking is enabled, CIL will silently allows such code, but does not insert
 * an explicit cast to covert the return value to an int, and will generate bad Call's with mismatched types.
 */
#pragma expect_cil_consistency_errors

int main(void) {
    int x = foo(); /* no prototype declared so the return type is mismatched */
    __ASSERT(x > 0);
    return 0;
}

unsigned char foo(void) {
    return 0xff;
}

