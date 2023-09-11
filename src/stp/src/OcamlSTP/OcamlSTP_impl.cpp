#include "../STPManager/STP.h"
#include "../printer/printers.h"
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <climits>

#define STRINGIFY(s) #s

extern "C" {
    #include <caml/alloc.h>
    #include <caml/custom.h>
    #include <caml/fail.h>
    #include <caml/memory.h>



    /*
     * STP query return values
     */
    enum validity {
        Invalid = 0,
        Valid = 1,
        Undecided = 2,
    };




    /*
     * Custom allocators (and finalizers) for STP objects
     */

    /* Maximum number of garbage before initiating garbage collection */
    const int STP_GC_RATE = 3000;

    /*
     * For validity checker, use a custom allocator which invokes
     * the garbage collector more aggresively
     */
    static void finalize_STP(value v) {
        BEEV::STP *stp = *(static_cast<BEEV::STP **>(Data_custom_val(v)));
        /* FIXME: STP's destructor should delete bm too */
        delete stp->bm;
        delete stp;
    }

    static int compare_STP(value v1, value v2) {
        BEEV::STP *stp1 = *(static_cast<BEEV::STP **>(Data_custom_val(v1)));
        BEEV::STP *stp2 = *(static_cast<BEEV::STP **>(Data_custom_val(v2)));
        if (stp1 < stp2) {
            return -1;
        } else if (stp1 == stp2) {
            return 0;
        } else {
            return 1;
        }
    }

    static intnat hash_STP(value v) {
        BEEV::STP *stp = *(static_cast<BEEV::STP **>(Data_custom_val(v)));
        return reinterpret_cast<intnat>(stp);
    }

    static struct custom_operations STP_ops = {
        const_cast<char *>("OcamlSTP.BEEV::STP"),
        finalize_STP,
        compare_STP,
        hash_STP,
        custom_serialize_default,
        custom_deserialize_default
    };

    static inline value c2ml_STP(BEEV::STP *input) {
        CAMLparam0();
        CAMLlocal1(out);
        out = caml_alloc_custom(&STP_ops, sizeof(BEEV::STP *), 1, STP_GC_RATE);
        *(static_cast<BEEV::STP **>(Data_custom_val(out))) = input;
        CAMLreturn(out);
    }

    static inline BEEV::STP *ml2c_STP(value input) {
        CAMLparam1(input);
        /* FIXME: eliminate globals rather than use this stupid hack to fix them */
        BEEV::STP *stp = *(static_cast<BEEV::STP **>(Data_custom_val(input)));
        BEEV::GlobalSTP = stp;
        BEEV::ParserBM = stp->bm;
        CAMLreturnT(BEEV::STP *, stp);
    }


    /*
     * For expressions, use the default allocator with the following finalizer
     */
    static void finalize_ASTNode(value v) {
        delete *(static_cast<BEEV::ASTNode **>(Data_custom_val(v)));
    }

    static int compare_ASTNode(value v1, value v2) {
        BEEV::ASTNode::element_type *p1 = (*(static_cast<BEEV::ASTNode **>(Data_custom_val(v1))))->get();
        BEEV::ASTNode::element_type *p2 = (*(static_cast<BEEV::ASTNode **>(Data_custom_val(v2))))->get();
        if (p1 < p2) {
            return -1;
        } else if (p1 > p2) {
            return 1;
        } else {
            return 0;
        }
    }

    static intnat hash_ASTNode(value v) {
        BEEV::ASTNode::element_type *p = (*(static_cast<BEEV::ASTNode **>(Data_custom_val(v))))->get();
        return reinterpret_cast<intnat>(p);
    }

    static struct custom_operations ASTNode_ops = {
        const_cast<char *>("OcamlSTP.ASTNode"),
        finalize_ASTNode,
        compare_ASTNode,
        hash_ASTNode,
        custom_serialize_default,
        custom_deserialize_default
    };

    static inline value c2ml_ASTNode(BEEV::ASTNode *input) {
        CAMLparam0();
        CAMLlocal1(out);
        out = caml_alloc_custom(&ASTNode_ops, sizeof(BEEV::ASTNode *), 0, 1);
        *(static_cast<BEEV::ASTNode **>(Data_custom_val(out))) = input;
        CAMLreturn(out);
    }

    static inline BEEV::ASTNode *ml2c_ASTNode(value input) {
        CAMLparam1(input);
        CAMLreturnT(BEEV::ASTNode *, *(static_cast<BEEV::ASTNode **>(Data_custom_val(input))));
    }




    /*
     * Initialization
     */
    CAMLprim value OcamlSTP_init(value unit) {
        CAMLparam1(unit);
        BEEV::vc_error_hdlr = caml_invalid_argument;
        CONSTANTBV::ErrCode c = CONSTANTBV::BitVector_Boot();
        if (c != 0) {
            caml_failwith(reinterpret_cast<const char *>(CONSTANTBV::BitVector_Error(c)));
        }
        CAMLreturn(Val_unit);
    }




    /*
     * STP constructor
     */
    static inline BEEV::STP *OcamlSTP_make_context_impl(void) {
        BEEV::STPMgr *bm = new BEEV::STPMgr();
        BEEV::Simplifier *simplifier = new BEEV::Simplifier(bm);
        BEEV::BVSolver *bvsolver = new BEEV::BVSolver(bm, simplifier);
        BEEV::ArrayTransformer *array_transformer = new BEEV::ArrayTransformer(bm, simplifier);
        BEEV::ToSAT *to_sat = new BEEV::ToSAT(bm);
        BEEV::AbsRefine_CounterExample *counter_example = new BEEV::AbsRefine_CounterExample(bm, simplifier, array_transformer);

        /* required by the simplifying BitBlaster (as of SVN r1296) */
        /* FIXME: STP's constructor should take a node factory as a parameter */
        SimplifyingNodeFactory *simp = new SimplifyingNodeFactory(*(bm->hashingNodeFactory), *bm);
        bm->defaultNodeFactory = simp;

        bm->UserFlags.construct_counterexample_flag = true;
        bm->UserFlags.check_counterexample_flag = true;
        return new BEEV::STP(bm, simplifier, bvsolver, array_transformer, to_sat, counter_example);
    }
    CAMLprim value OcamlSTP_make_context(value unit) {
        CAMLparam1(unit);
        CAMLreturn(c2ml_STP(OcamlSTP_make_context_impl()));
    }




    /*
     * Options
     */
    CAMLprim value OcamlSTP_set_seed(value v_stp, value v_seed) {
        CAMLparam2(v_stp, v_seed);

        BEEV::STP *stp = ml2c_STP(v_stp);

        if (Int_val(v_seed) == 0) { /* None */
            stp->bm->UserFlags.random_seed_flag = false;
        } else {
            intnat seed = Long_val(Field(v_seed, 0));
            stp->bm->UserFlags.random_seed_flag = true;
            stp->bm->UserFlags.random_seed = seed;
        }
        CAMLreturn(Val_unit);
    }




    /*
     * Helpers
     */
    static inline unsigned check_value_width(const BEEV::ASTNode *left, const BEEV::ASTNode *right) {
        if (left->GetValueWidth() != right->GetValueWidth()) {
            caml_invalid_argument("operands must be of equal width");
        }
        return left->GetValueWidth();
    }




    /*
     * Variable constructors
     */
    static inline BEEV::ASTNode *OcamlSTP_bool_var_impl(BEEV::STP *stp, const char *name) {
        if (stp->bm->LookupSymbol(name)) {
            BEEV::ASTNode symbol = stp->bm->LookupOrCreateSymbol(name);
            if (symbol.GetIndexWidth() != 0 || symbol.GetValueWidth() != 0) {
                caml_invalid_argument("non-boolean symbol with the same name exists");
            }
            return new BEEV::ASTNode(symbol);
        }
        return new BEEV::ASTNode(stp->bm->CreateSymbol(name, 0, 0));
    }
    CAMLprim value OcamlSTP_bool_var(value stp, value name) {
        CAMLparam2(stp, name);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_var_impl(ml2c_STP(stp), String_val(name))));
    }


    BEEV::ASTNode *OcamlSTP_bv_var_impl(BEEV::STP *stp, const char *name, const intnat width) {
        if (width <= 0) {
            caml_invalid_argument("width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateSymbol takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        if (stp->bm->LookupSymbol(name)) {
            BEEV::ASTNode symbol = stp->bm->LookupOrCreateSymbol(name);
            if (symbol.GetIndexWidth() != 0 || symbol.GetValueWidth() != width) {
                caml_invalid_argument("non-bitvector symbol or bitvector symbol of different width with the same name exists");
            }
            return new BEEV::ASTNode(symbol);
        }
        return new BEEV::ASTNode(stp->bm->CreateSymbol(name, 0, width));
    }
    CAMLprim value OcamlSTP_bv_var(value stp, value name, value width) {
        CAMLparam3(stp, name, width);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_var_impl(ml2c_STP(stp), String_val(name), Long_val(width))));
    }


    static inline BEEV::ASTNode *OcamlSTP_array_var_impl(BEEV::STP *stp, const char *name, const intnat index_width, const intnat val_width) {
        if (index_width <= 0) {
            caml_invalid_argument("index_width must be greater than zero");
        }
        if (val_width <= 0) {
            caml_invalid_argument("val_width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateSymbol takes an unsigned int) */
        if (index_width > UINT_MAX) {
            caml_invalid_argument("index_width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        if (val_width > UINT_MAX) {
            caml_invalid_argument("val_width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        if (stp->bm->LookupSymbol(name)) {
            BEEV::ASTNode symbol = stp->bm->LookupOrCreateSymbol(name);
            if (symbol.GetIndexWidth() != index_width || symbol.GetValueWidth() != val_width) {
                caml_invalid_argument("non-array symbol or array symbol of different widths with the same name exists");
            }
            return new BEEV::ASTNode(symbol);
        }
        return new BEEV::ASTNode(stp->bm->CreateSymbol(name, index_width, val_width));
    }
    CAMLprim value OcamlSTP_array_var(value stp, value name, value index_width, value val_width) {
        CAMLparam4(stp, name, index_width, val_width);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_array_var_impl(ml2c_STP(stp), String_val(name), Long_val(index_width), Long_val(val_width))));
    }




    /*
     * Constant constructors
     */
    static inline BEEV::ASTNode *OcamlSTP_bool_true_impl(BEEV::STP *stp) {
        return new BEEV::ASTNode(stp->bm->ASTTrue);
    }
    CAMLprim value OcamlSTP_bool_true(value stp) {
        CAMLparam1(stp);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_true_impl(ml2c_STP(stp))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_false_impl(BEEV::STP *stp) {
        return new BEEV::ASTNode(stp->bm->ASTFalse);
    }
    CAMLprim value OcamlSTP_bool_false(value stp) {
        CAMLparam1(stp);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_false_impl(ml2c_STP(stp))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_of_int_impl(BEEV::STP *stp, const intnat width, const intnat val) {
        if (width <= 0) {
            caml_invalid_argument("width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        return new BEEV::ASTNode(stp->bm->CreateBVConst(width, val));
    }
    CAMLprim value OcamlSTP_bv_of_int(value stp, value width, value val) {
        CAMLparam3(stp, width, val);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_of_int_impl(ml2c_STP(stp), Long_val(width), Long_val(val))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_of_int32_impl(BEEV::STP *stp, const intnat width, const intnat val) {
        if (width <= 0) {
            caml_invalid_argument("width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        return new BEEV::ASTNode(stp->bm->CreateBVConst(width, val));
    }
    CAMLprim value OcamlSTP_bv_of_int32(value stp, value width, value val) {
        CAMLparam3(stp, width, val);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_of_int32_impl(ml2c_STP(stp), Long_val(width), Int32_val(val))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_of_int64_impl(BEEV::STP *stp, const intnat width, const int64 val) {
        if (width <= 0) {
            caml_invalid_argument("width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        return new BEEV::ASTNode(stp->bm->CreateBVConst(width, val));
    }
    CAMLprim value OcamlSTP_bv_of_int64(value stp, value width, value val) {
        CAMLparam3(stp, width, val);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_of_int64_impl(ml2c_STP(stp), Long_val(width), Int64_val(val))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_of_int_string_impl(BEEV::STP *stp, const intnat width, const char *val_string, const intnat base) {
        if (width <= 0) {
            caml_invalid_argument("width must be greater than zero");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        std::string str(val_string);
        return new BEEV::ASTNode(stp->bm->CreateBVConst(str, base, width));
    }
    CAMLprim value OcamlSTP_bv_of_int_string(value stp, value width, value val_string, value base) {
        CAMLparam4(stp, width, val_string, base);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_of_int_string_impl(ml2c_STP(stp), Long_val(width), String_val(val_string), Long_val(base))));
    }




    /*
     * Boolean operations
     */
    static inline BEEV::ASTNode *OcamlSTP_bool_not_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::NOT, *expr));
    }
    CAMLprim value OcamlSTP_bool_not(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_not_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_and_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::AND, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_and(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_and_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_or_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::OR, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_or(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_or_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_nand_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::NAND, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_nand(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_nand_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_nor_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::NOR, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_nor(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_nor_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_xor_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::XOR, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_xor(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_xor_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_iff_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::IFF, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_iff(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_iff_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bool_implies_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::IMPLIES, *left, *right));
    }
    CAMLprim value OcamlSTP_bool_implies(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bool_implies_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }




    /*
     * Bitvector arithmetic operations
     */
    static inline BEEV::ASTNode *OcamlSTP_bv_neg_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVUMINUS, expr->GetValueWidth(), *expr));
    }
    CAMLprim value OcamlSTP_bv_neg(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_neg_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_add_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVPLUS, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_add(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_add_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_sub_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVSUB, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_sub(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_sub_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_mul_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVMULT, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_mul(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_mul_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_div_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVDIV, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_div(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_div_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_rem_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVMOD, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_rem(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_rem_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_div_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::SBVDIV, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_div(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_div_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_rem_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::SBVREM, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_rem(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_rem_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_mod_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::SBVMOD, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_mod(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_mod_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }




    /*
     * Bitvector bitwise operations
     */
    static inline BEEV::ASTNode *OcamlSTP_bv_shift_left_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVLEFTSHIFT, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_shift_left(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_shift_left_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_shift_right_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVRIGHTSHIFT, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_shift_right(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_shift_right_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_shift_right_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVSRSHIFT, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_shift_right(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_shift_right_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_not_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVNEG, expr->GetValueWidth(), *expr));
    }
    CAMLprim value OcamlSTP_bv_not(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_not_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_and_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVAND, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_and(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_and_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_or_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVOR, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_or(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_or_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_xor_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVXOR, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_xor(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_xor_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }

/* Buggy
    static inline BEEV::ASTNode *OcamlSTP_bv_nand_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVNAND, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_nand(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_nand_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_nor_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVNOR, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_nor(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_nor_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_xnor_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVXNOR, check_value_width(left, right), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_xnor(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_xnor_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }
*/



    /*
     * Bitvector operations
     */
    static inline BEEV::ASTNode *OcamlSTP_bv_get_bit_impl(BEEV::STP *stp, BEEV::ASTNode *expr, intnat bit) {
        if (bit < 0) {
            caml_invalid_argument("bit must be greater or equal to zero");
        }
        if (bit >= expr->GetValueWidth()) {
            caml_invalid_argument("bit must be less than the operand width");
        }
        BEEV::ASTNode bv_bit = stp->bm->CreateBVConst(32, bit);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BOOLEXTRACT, *expr, bv_bit));
    }
    CAMLprim value OcamlSTP_bv_get_bit(value stp, value expr, value bit) {
        CAMLparam3(stp, expr, bit);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_get_bit_impl(ml2c_STP(stp), ml2c_ASTNode(expr), Long_val(bit))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_concat_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVCONCAT, left->GetValueWidth() + right->GetValueWidth(), *left, *right));
    }
    CAMLprim value OcamlSTP_bv_concat(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_concat_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_extract_impl(BEEV::STP *stp, BEEV::ASTNode *expr, intnat high_bit, intnat low_bit) {
        if (high_bit < 0) {
            caml_invalid_argument("high bit must be greater or equal to zero");
        }
        if (low_bit < 0) {
            caml_invalid_argument("low bit must be greater or equal to zero");
        }
        if (high_bit < low_bit) {
            caml_invalid_argument("high bit must be greater or equal to the low bit");
        }
        if (high_bit >= expr->GetValueWidth()) {
            caml_invalid_argument("high bit must be less than the operand width");
        }
        BEEV::ASTNode bv_high = stp->bm->CreateBVConst(32, high_bit);
        BEEV::ASTNode bv_low = stp->bm->CreateBVConst(32, low_bit);
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVEXTRACT, high_bit - low_bit + 1, *expr, bv_high, bv_low));
    }
    CAMLprim value OcamlSTP_bv_extract(value stp, value expr, value high_bit, value low_bit) {
        CAMLparam4(stp, expr, high_bit, low_bit);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_extract_impl(ml2c_STP(stp), ml2c_ASTNode(expr), Long_val(high_bit), Long_val(low_bit))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_zero_extend_impl(BEEV::STP *stp, BEEV::ASTNode *expr, intnat width) {
        if (width <= expr->GetValueWidth()) {
            caml_invalid_argument("width must be greater than the operand width");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        BEEV::ASTNode bv_width = stp->bm->CreateBVConst(32, width);
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVZX, width, *expr, bv_width));
    }
    CAMLprim value OcamlSTP_bv_zero_extend(value stp, value expr, value width) {
        CAMLparam3(stp, expr, width);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_zero_extend_impl(ml2c_STP(stp), ml2c_ASTNode(expr), Long_val(width))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_sign_extend_impl(BEEV::STP *stp, BEEV::ASTNode *expr, intnat width) {
        if (width <= expr->GetValueWidth()) {
            caml_invalid_argument("width must be greater than the operand width");
        }
        /* Ocaml's int type corresponds to signed long in C, whereas CreateBVConst takes an unsigned int) */
        if (width > UINT_MAX) {
            caml_invalid_argument("width must be less or equal to" STRINGIFY(UINT_MAX));
        }
        BEEV::ASTNode bv_width = stp->bm->CreateBVConst(32, width);
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::BVSX, width, *expr, bv_width));
    }
    CAMLprim value OcamlSTP_bv_sign_extend(value stp, value expr, value width) {
        CAMLparam3(stp, expr, width);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_sign_extend_impl(ml2c_STP(stp), ml2c_ASTNode(expr), Long_val(width))));
    }




    /*
     * Bitvector comparison
     */
    static inline BEEV::ASTNode *OcamlSTP_bv_eq_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::EQ, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_eq(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_eq_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_lt_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVLT, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_lt(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_lt_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_le_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVLE, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_le(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_le_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_gt_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVGT, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_gt(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_gt_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_ge_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVGE, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_ge(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_ge_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_lt_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVSLT, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_lt(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_lt_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_le_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVSLE, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_le(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_le_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_gt_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVSGT, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_gt(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_gt_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }


    static inline BEEV::ASTNode *OcamlSTP_bv_signed_ge_impl(BEEV::STP *stp, BEEV::ASTNode *left, BEEV::ASTNode *right) {
        check_value_width(left, right);
        return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::BVSGE, *left, *right));
    }
    CAMLprim value OcamlSTP_bv_signed_ge(value stp, value left, value right) {
        CAMLparam3(stp, left, right);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_bv_signed_ge_impl(ml2c_STP(stp), ml2c_ASTNode(left), ml2c_ASTNode(right))));
    }




    /*
     * Array operations
     */
    static inline BEEV::ASTNode *OcamlSTP_array_read_impl(BEEV::STP *stp, BEEV::ASTNode *array, BEEV::ASTNode *index) {
        if (index->GetValueWidth() != array->GetIndexWidth()) {
            caml_invalid_argument("index width must equal to the array index width");
        }
        return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::READ, array->GetValueWidth(), *array, *index));
    }
    CAMLprim value OcamlSTP_array_read(value stp, value array, value index) {
        CAMLparam3(stp, array, index);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_array_read_impl(ml2c_STP(stp), ml2c_ASTNode(array), ml2c_ASTNode(index))));
    }


    static inline BEEV::ASTNode *OcamlSTP_array_write_impl(BEEV::STP *stp, BEEV::ASTNode *array, BEEV::ASTNode *index, BEEV::ASTNode *val) {
        if (index->GetValueWidth() != array->GetIndexWidth()) {
            caml_invalid_argument("index width must equal to the array index width");
        }
        if (val->GetValueWidth() != array->GetValueWidth()) {
            caml_invalid_argument("value width must equal to the array value width");
        }
        return new BEEV::ASTNode(stp->bm->CreateArrayTerm(BEEV::WRITE, array->GetIndexWidth(), array->GetValueWidth(), *array, *index, *val));
    }
    CAMLprim value OcamlSTP_array_write(value stp, value array, value index, value val) {
        CAMLparam4(stp, array, index, val);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_array_write_impl(ml2c_STP(stp), ml2c_ASTNode(array), ml2c_ASTNode(index), ml2c_ASTNode(val))));
    }




    /*
     * Conditionals
     */
    static inline BEEV::ASTNode * OcamlSTP_ite_imp(BEEV::STP *stp, BEEV::ASTNode *cond, BEEV::ASTNode *then, BEEV::ASTNode *els) {
        unsigned val_width, index_width;

        val_width = check_value_width(then, els);
        if (then->GetIndexWidth() != els->GetIndexWidth()) {
            caml_invalid_argument("operand indices must be of equal width");
        }
        index_width = then->GetIndexWidth();

        if (val_width == 0) {
            return new BEEV::ASTNode(stp->bm->CreateNode(BEEV::ITE, *cond, *then, *els));
        } else if (index_width == 0) {
            return new BEEV::ASTNode(stp->bm->CreateTerm(BEEV::ITE, val_width, *cond, *then, *els));
        } else {
            return new BEEV::ASTNode(stp->bm->CreateArrayTerm(BEEV::ITE, index_width, val_width, *cond, *then, *els));
        }
    }
    CAMLprim value OcamlSTP_ite(value stp, value cond, value then, value els) {
        CAMLparam4(stp, cond, then, els);
        CAMLreturn(c2ml_ASTNode(OcamlSTP_ite_imp(ml2c_STP(stp), ml2c_ASTNode(cond), ml2c_ASTNode(then), ml2c_ASTNode(els))));
    }




    /*
     * Validity checking
     */
    static inline void OcamlSTP_vc_assert_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        stp->bm->AddAssert(*expr, 0);
    }
    CAMLprim value OcamlSTP_vc_assert(value stp, value expr) {
        CAMLparam2(stp, expr);
        OcamlSTP_vc_assert_impl(ml2c_STP(stp), ml2c_ASTNode(expr));
        CAMLreturn(Val_unit);
    }


    static inline void OcamlSTP_vc_push_impl(BEEV::STP *stp) {
        stp->bm->Push();
    }
    CAMLprim value OcamlSTP_vc_push(value stp) {
        CAMLparam1(stp);
        OcamlSTP_vc_push_impl(ml2c_STP(stp));
        CAMLreturn(Val_unit);
    }


    static inline void OcamlSTP_vc_pop_impl(BEEV::STP *stp) {
        stp->bm->Pop();
    }
    CAMLprim value OcamlSTP_vc_pop(value stp) {
        CAMLparam1(stp);
        OcamlSTP_vc_pop_impl(ml2c_STP(stp));
        CAMLreturn(Val_unit);
    }


    static inline BEEV::SOLVER_RETURN_TYPE OcamlSTP_vc_query_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        BEEV::SOLVER_RETURN_TYPE result;

         /* FIXME: Is this the best place to clear all tables? Consecutive queries won't be correct otherwise.
         * If so, it'll be better to move this to the top of TopLevelSTP. */
        stp->ClearAllTables();

        /* FIXME: The logic below probably should also be moved to the top of TopLevelSTP */
        const BEEV::ASTVec asserts = stp->bm->GetAsserts();
        if (asserts.empty()) {
            result = stp->TopLevelSTP(stp->bm->CreateNode(BEEV::TRUE), *expr);
        } else if (asserts.size() == 1) {
            result = stp->TopLevelSTP(asserts[0], *expr);
        } else {
            result = stp->TopLevelSTP(stp->bm->CreateNode(BEEV::AND, asserts), *expr);
        }

        /* FIXME: STP doesn't seem to set the ValidFlag unless it's printed, so without the next line,
         * vc_get_counterexamples below may return spurious results */
        stp->bm->ValidFlag = result == BEEV::SOLVER_VALID;
        return result;
    }
    CAMLprim value OcamlSTP_vc_query(value stp, value expr) {
        CAMLparam2(stp, expr);
        BEEV::SOLVER_RETURN_TYPE result = OcamlSTP_vc_query_impl(ml2c_STP(stp), ml2c_ASTNode(expr));
        switch (result)  {
            case BEEV::SOLVER_INVALID: CAMLreturn(Val_int(Invalid));
            case BEEV::SOLVER_VALID: CAMLreturn(Val_int(Valid));
            case BEEV::SOLVER_UNDECIDED: CAMLreturn(Val_int(Undecided));
            default: caml_failwith("unknown result"); CAMLreturn(Val_int(Undecided));
        }
    }


    CAMLprim value OcamlSTP_vc_get_asserts(value v_stp) {
        CAMLparam1(v_stp);
        CAMLlocal2(result, r);
        BEEV::STP *stp = ml2c_STP(v_stp); /* to fix the context */

        result = Val_int(0);
        BEEV::ASTVec asserts_vec = stp->bm->GetAsserts();
        for (BEEV::ASTVec::reverse_iterator rit = asserts_vec.rbegin(); rit < asserts_vec.rend(); ++rit) {
            BEEV::ASTNode *expr = new BEEV::ASTNode(*rit);
            r = caml_alloc_small(2, 0);
            Field(r, 0) = c2ml_ASTNode(expr);
            Field(r, 1) = result;
            result = r;
        }
        CAMLreturn(result);
    }


    CAMLprim value OcamlSTP_vc_get_counterexample(value v_stp, value v_query) {
        CAMLparam2(v_stp, v_query);
        CAMLlocal1(result);

        BEEV::STP *stp = ml2c_STP(v_stp);
        BEEV::ASTNode *query = ml2c_ASTNode(v_query);

        /* FIXME: the first argument to GetCounterExample seems to be ignored */
        if (stp->bm->ValidFlag) {
            result = Val_int(0); /* None */
        } else {
            BEEV::ASTNode counterexample = stp->Ctr_Example->GetCounterExample(false, *query);
            if (query->GetValueWidth() != counterexample.GetValueWidth()
                    || query->GetIndexWidth() != counterexample.GetIndexWidth()) {
                caml_invalid_argument("query type does not match result type");
            }
            BEEV::ASTNode *r = new BEEV::ASTNode(counterexample);
            result = caml_alloc_small(1, 0); /* Some */
            Field(result, 0) = c2ml_ASTNode(r);
        }
        CAMLreturn(result);
    }




    /*
     * Metadata query
     */
    static inline int OcamlSTP_bv_width_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return expr->GetValueWidth();
    }
    CAMLprim value OcamlSTP_bv_width(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(Val_long(OcamlSTP_bv_width_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline int OcamlSTP_array_value_width_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return expr->GetValueWidth();
    }
    CAMLprim value OcamlSTP_array_value_width(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(Val_long(OcamlSTP_array_value_width_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline int OcamlSTP_array_index_width_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        return expr->GetIndexWidth();
    }
    CAMLprim value OcamlSTP_array_index_width(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(Val_long(OcamlSTP_array_index_width_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }




    /*
     * Conversion
     */
    static inline bool OcamlSTP_to_bool_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        switch (expr->GetKind()) {
            case BEEV::TRUE:
                return true;
            case BEEV::FALSE:
                return false;
            default:
                caml_invalid_argument("not a constant bool");
        }
    }
    CAMLprim value OcamlSTP_to_bool(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(Val_bool(OcamlSTP_to_bool_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline intnat OcamlSTP_to_int_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        const int intnat_width = 8 * sizeof(intnat) - 1;
        if (expr->GetValueWidth() > intnat_width) {
            caml_invalid_argument("constant bitvector width greater than int width");
        }
        BEEV::CBV cbv = (static_cast<BEEV::ASTBVConst *>(expr->get()))->GetBVConst();
        return CONSTANTBV::BitVector_Chunk_Read(cbv, intnat_width, 0);
    }
    CAMLprim value OcamlSTP_to_int(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(Val_long(OcamlSTP_to_int_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline int32 OcamlSTP_to_int32_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        if (expr->GetValueWidth() > 32) {
            caml_invalid_argument("constant bitvector width greater than int32 width");
        }
        BEEV::CBV cbv = (static_cast<BEEV::ASTBVConst *>(expr->get()))->GetBVConst();
        return CONSTANTBV::BitVector_Chunk_Read(cbv, 32, 0);
    }
    CAMLprim value OcamlSTP_to_int32(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(caml_copy_int32(OcamlSTP_to_int32_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    static inline int64 OcamlSTP_to_int64_impl(BEEV::STP *stp, BEEV::ASTNode *expr) {
        if (expr->GetValueWidth() > 64) {
            caml_invalid_argument("constant bitvector width greater than int64 width");
        }
        BEEV::CBV cbv = (static_cast<BEEV::ASTBVConst *>(expr->get()))->GetBVConst();
        return
            (CONSTANTBV::BitVector_Chunk_Read(cbv, 32, 32) << 32)
            | (CONSTANTBV::BitVector_Chunk_Read(cbv, 32, 0));
    }
    CAMLprim value OcamlSTP_to_int64(value stp, value expr) {
        CAMLparam2(stp, expr);
        CAMLreturn(caml_copy_int64(OcamlSTP_to_int64_impl(ml2c_STP(stp), ml2c_ASTNode(expr))));
    }


    CAMLprim value OcamlSTP_to_string(value v_stp, value v_expr) {
        CAMLparam2(v_stp, v_expr);
        BEEV::STP *stp = ml2c_STP(v_stp); /* to fix the context */
        BEEV::ASTNode *expr = ml2c_ASTNode(v_expr);

        std::stringstream stream;
        expr->PL_Print(stream, 0);
        CAMLreturn(caml_copy_string(stream.str().c_str()));
    }
}

