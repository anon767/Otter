
(* mix options *)
let opt_symbolic_top = ref false
let opt_args = ref []


(* mix analysis modules *)
module TypedInterpreter = TypedBlock.Interpreter (Config.TypedBlockConfig)
module SymbolicInterpreter = SymbolicBlock.Interpreter (Config.SymbolicBlockConfig)
module TypedSymbolicSwitcher = TypedSymbolic.Switcher (Config.TypedBlockConfig) (Config.SymbolicBlockConfig)

let dispatcher file =
    let analysis_register = [
        TypedInterpreter.dispatch;
        SymbolicInterpreter.dispatch;
    ] in
    let switch_register = [
        TypedSymbolicSwitcher.dispatch;
    ] in
    let terminal = (fun file stack -> failwith "Can't dispatch call") in

    (* chained in reverse to ensure that analysis occurs before switch *)
    let switcher = List.fold_left (fun d f -> f d) terminal switch_register in
    let dispatcher = List.fold_left (fun d f -> f d) switcher analysis_register in
    dispatcher file


(* Cil setup *)
let init_cil () =
    TypedInterpreter.init_cil ();
    SymbolicInterpreter.init_cil ()

let prepare_file file =
    TypedInterpreter.prepare_file file;
    SymbolicInterpreter.prepare_file file


(* mix dispatch loop *)
let rec dispatch_loop file = function
    | `Done -> ()
    | call -> dispatch_loop file (dispatcher file call)


(* mix driver *)
let doit file =
    prepare_file file;

    if not !opt_symbolic_top then
        dispatch_loop file (TypedInterpreter.exec file)
    else
        dispatch_loop file (SymbolicInterpreter.exec file !opt_args)


(* Cil feature description *)
let description = {
    Cil.fd_name = "mix";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "check type qualifier with mix analysis";
    Cil.fd_extraopt = [
        ("--mix-symbolic-top", Arg.Set opt_symbolic_top,
            " Run symbolic analysis at the outermost level (default is typed)");
        ("--mix-arg", Arg.String (fun arg -> opt_args := !opt_args @ [arg]),
            "<arg> Run with command line argument <arg>\n\
             \t\t\t\t(This option can be repeated to give multiple arguments.)");
    ];
    Cil.fd_post_check = false;
    Cil.fd_doit = doit
}

