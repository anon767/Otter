open OtterBytes

let init_methods : (string*(unit -> Bytes.byte)) list = [
    "zero", (fun () -> Bytes.byte__zero);
    "undefined", (fun () -> Bytes.byte__undef);
    "symbolic", Bytes.make_Byte_Symbolic;
]

let get_init_method init_method = List.assoc init_method init_methods

let init_global = ref (List.assoc "zero" init_methods)
let init_local = ref (List.assoc "undefined" init_methods)
let init_malloc = ref (List.assoc "undefined" init_methods)

(**
 *  Command-line options
 *)
let options = [
    "--init-local",
        Arg.Symbol (fst (List.split init_methods), fun name -> init_local := get_init_method name),
        " Set the default init method for local variables (default: " ^ (fst (List.find (fun (_, x) -> x == !init_local) init_methods)) ^ ")";
    "--init-malloc",
        Arg.Symbol (fst (List.split init_methods), fun name -> init_local := get_init_method name),
        " Set the default init method for malloc-ed memory (default: " ^ (fst (List.find (fun (_, x) -> x == !init_malloc) init_methods)) ^ ")";
]

