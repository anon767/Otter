#include <caml/mlvalues.h>
#include <caml/custom.h>
#include "libstp.h"


/*
 * custom allocators (and finalizers) for STP objects
 */

/* maximum number of garbage before initiating garbage collection */
#define VC_GC_RATE 3000

/*
 * For validity checker, use a custom allocator which invokes
 * the garbage collector more aggresively.
 */
static void finalize_Vc(value v) {
    vc_Destroy(*((Vc *)Data_custom_val(v)));
}

static struct custom_operations vc_ops = {
    "ocamlstp.Vc",
    finalize_Vc,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

value c2ml_Vc(Vc *input) {
    value out = caml_alloc_custom(&vc_ops, sizeof(Vc), 1, VC_GC_RATE);
    *((Vc *)Data_custom_val(out)) = *input;
    return out;
}

/* map ML value back to (void *) pointer */
void ml2c_Vc(value input, void **output) {
    *output = *((void **)Data_custom_val(input));
}


/*
 * For expressions and types, use the default allocator with the
 * following finalizers
 */
void finalize_Expr(Expr * v) {
    vc_DeleteExpr(*v);
}

void finalize_Typ(Typ * v) {
    //vc_DeleteType(*v);
}



