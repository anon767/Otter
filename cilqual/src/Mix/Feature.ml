
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
     *         - chain may be called by the X.dispatch function to delegate to the next X.dispatch function in the
     *           chain, e.g., if it cannot handle the next work block
     *         - dispatch may be called by the X.dispatch function to delegate to the first X.dispatch function in the
     *           chain, e.g., if it needs to analyze a new work block
     *
     *     - stack is an explicit stack of open polymorphic variants with the type : [> ] list
     *         - it is mainly used for operations that require stack inspection, e.g., to detect recursion
     *         - the X.dispatch functions are responsible for pushing/popping the stack when creating/completing work
     *           blocks
     *
     *     - work is a continuation-based implicit stack with an open polymorphic variants type : [> ]
     *         - typically, the polymorphic variants are of the form `X (block_input, k) where k is a continuation with
     *           the type : block_result -> result
     *         - the X.dispatch functions can "push" the stack by creating a new work `X (block_input', k') where k'
     *           is a continuation function that eventually calls k to "pop" the stack, and calling chain or dispatch
     *           with the new work block
     *         - work is passed as input to X.dispatch which can decide whether to analyze it or to delegate to another
     *           X.dispatch by pattern matching the variant tag `X
     *         - `X/block_input/block_result are specific to each X.dispatch since different analysis consume or
     *           produce different data
     *
     *     - result is the final result of the analysis and is determined by the analysis that generated the very
     *       first work block
     *)

    (* TODO: merge work into stack, since an explicit stack is required for analysis such as recursion detection,
     *       and having two stacks to pass around is more hassle *)

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

