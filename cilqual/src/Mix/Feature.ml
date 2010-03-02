
(* mix options *)
let opt_symbolic_top = ref false
let opt_args = ref []


(* mix analysis modules *)
module TypedInterpreter = TypedBlock.Interpreter (Config.TypedBlockConfig)
module SymbolicInterpreter = SymbolicBlock.Interpreter (Config.SymbolicBlockConfig)
module TypedSymbolicSwitcher = TypedSymbolic.Switcher (Config.TypedBlockConfig) (Config.SymbolicBlockConfig)
module SymbolicTypedSwitcher = SymbolicTyped.Switcher (Config.SymbolicBlockConfig) (Config.TypedBlockConfig)

let dispatcher s x =
    (*
     * This function composes a list of X.dispatch functions into a delegation chain.
     *
     * X.dispatch functions have the type : chain -> dispatch -> stack -> work -> result
     *
     *     - chain and dispatch are functions with the type : stack -> work -> result
     *         - chain may be called by the X.dispatch function to delegate to the next X.dispatch function,
     *           e.g., if it cannot handle the next work block
     *         - dispatch may be called by the X.dispatch function to delegate to the first X.dispatch function,
     *           e.g., if it needs to analyze a new work block
     *
     *     - stack is an explicit stack of open polymorphic variants with the type : [> ] list
     *         - it is mainly used for operations requiring stack inspection
     *         - the X.dispatch functions are responsible to pushing/popping the stack when creating/completing the
     *           analysis of new work blocks 
     *
     *     - work is an continuation-based implicit stack of open polymorphic variants with the type : [> ]
     *         - typically the polymorphic variants of the form `X (block_input, k) where k is a continuation with the
     *           type : block_result -> result
     *         - work as parameter passed as input to X.dispatch which decides whether to analyze it or to delegate to
     *           the next X.dispatch
     *
     *     - block_input/block_result are specific to each X.dispatch since different analysis consume or produce
     *       different data
     *
     *     - result is a the final result of the analysis, and is specific to the very first work block
     *)

    (* TODO: merge work into stack, since an explicit stack is required for analysis such as recursion detection,
     *       and two stacks is more hassle to maintain *)

    let analysis_register = [
        TypedInterpreter.dispatch;
        SymbolicInterpreter.dispatch;
    ] in
    let switch_register = [
        TypedSymbolicSwitcher.dispatch;
        SymbolicTypedSwitcher.dispatch;
    ] in
    let terminal dispatch x = failwith "Can't dispatch call" in

    (* compose analyzers and switchers into a chain *)
    let dispatcher = List.fold_left (fun f g d -> g (f d) d) terminal switch_register in
    let dispatcher = List.fold_left (fun f g d -> g (f d) d) dispatcher analysis_register in
    dispatcher s x


(* Cil setup *)
let init_cil () =
    TypedInterpreter.init_cil ();
    SymbolicInterpreter.init_cil ()

let prepare_file file =
    TypedInterpreter.prepare_file file;
    SymbolicInterpreter.prepare_file file;
    SwitchingUtil.prepare_file file


(* mix dispatch loop *)
let dispatch_loop x =
    let rec fix f s x = f (fix f) s x in
    fix dispatcher [] x


(* mix driver *)
let doit file =
    prepare_file file;

    if not !opt_symbolic_top then begin
        ignore (dispatch_loop (TypedInterpreter.exec file))
    end else begin
        ignore (dispatch_loop (SymbolicInterpreter.exec file !opt_args))
    end


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

