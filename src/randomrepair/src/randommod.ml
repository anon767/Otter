
(* by default, don't show anything *)
let opt_showinput = ref false
let opt_show = ref (nan)
let opt_showN = ref (-1)
let opt_showall = ref false
let opt_showallN = ref false
let opt_histogram = ref (-1)
let opt_showpatchtree = ref false



(* utility functions to detect Cil-introduced temporaries *)
let cilTempRegex = Str.regexp "^__cil_tmp"
let isVarCilTemp { Cil.vname=vname } = Str.string_match cilTempRegex vname 0



(* AST node filters; must be stateless *)
class virtual patchmakingFilter =
    object
        method virtual name : string
        method vvdec  (_ : Cil.varinfo) = true
        method vvrbl  (_ : Cil.varinfo) = true
        method vexpr  (_ : Cil.exp)     = true
        method vlval  (_ : Cil.lval)    = true
        method vinst  (_ : Cil.instr)   = true
        method vstmt  (_ : Cil.stmt)    = true
        method vblock (_ : Cil.block)   = true
        method vfunc  (_ : Cil.fundec)  = true

        (*
        method voffs (o:offset)
        method vinitoffs (o:offset)
        method vglob (g:global)
        method vinit (forg: varinfo) (off: offset) (i:init)
        method vtype (t:typ)
        method vattr (a: attribute)
        method vattrparam (a: attrparam)
        *)
    end

(* Filter for Cil-introduced structures *)
let cilFilter =
    object
        inherit patchmakingFilter
        method name = "cilFilter"
        (* skip Cil-introduced temporary variables *)
        method vvdec v = not (isVarCilTemp v)
        method vvrbl v = not (isVarCilTemp v)
        method vexpr = function
            | Cil.Lval(Cil.Var(v), _) -> not (isVarCilTemp v)
            | _ -> true
        method vlval = function
            | Cil.Var(v), _ -> not (isVarCilTemp v)
            | _ -> true
        method vinst = function
            (* skip Cil-introduced "__cil_tmp = 0/1" due to && || operator expansion;
               these should never be touched by patches, as they represent true/false branch conditions *)
            | Cil.Set((Cil.Var(v), _), e, _) -> not (isVarCilTemp v && (e = Cil.one || e = Cil.zero))
            | _ -> true
    end


(* visitor to generate candidate patches; may contain state (during initial pass only?) *)
class virtual patchmakingVisitor =
    object
        method virtual name : string
        method vvdec  (_ : Cil.varinfo) : Cil.varinfo list = []
        method vvrbl  (_ : Cil.varinfo) : Cil.varinfo list = []
        method vexpr  (_ : Cil.exp)     : Cil.exp list     = []
        method vlval  (_ : Cil.lval)    : Cil.lval list    = []
        method vinst  (_ : Cil.instr)   : Cil.instr list   = []
        method vstmt  (_ : Cil.stmt)    : Cil.stmt list    = []
        method vblock (_ : Cil.block)   : Cil.block list   = []
        method vfunc  (_ : Cil.fundec)  : Cil.fundec list  = []
    end



(* patchmakers registry *)
type 'a factory = unit -> 'a
type patchmakerEntry = { patchmakerFactory : patchmakingVisitor factory; mutable patchmakerWeight : float }
let allPatchmakers : patchmakerEntry list ref = ref []
(* registering function is at the end of the file, since it has to be after Cil.feature *)


(*
type pp = [   `Top | `Patchmaker of patchmakingVisitor | `Decl of Cil.varinfo | `Var of Cil.varinfo | `Expr of Cil.exp | `Lval of Cil.lval
            | `Inst of Cil.instr | `Stmt of Cil.stmt | `Block of Cil.block | `Fun of Cil.fundec ]
*)

(* patch-tree data structure associating patches to ast nodes *)
class ['data] patchtree mass data =
    object (self)
        inherit ['data] Dtree.tree mass data as super
        method private printer ff =
            let printf s = Format.fprintf ff ("%3d, %3.1f, %4.1f "^^s) self#count self#weight self#mass in
            let dprint s d x =
                let lines = Str.split (Str.regexp "\n") (Pretty.sprint 0 (d () x)) in
                printf s (fun ff -> Format.fprintf ff "%s" (List.hd lines); List.iter (Format.fprintf ff "@\n%s") (List.tl lines))
            in
            match data with
                | `Top -> printf "Top"
                | `Patchmaker pm -> printf "Patchmaker @[%s@]" pm#name
                | `Decl v  -> dprint "Decl @[%t %s@]" Cil.d_type v.Cil.vtype v.Cil.vname
                | `Var v   -> printf "Var @[%s@]" v.Cil.vname
                | `Expr e  -> dprint "Expr @[%t@]" Cil.d_exp e
                | `Lval l  -> dprint "Lval @[%t@]" Cil.d_lval l
                | `Inst i  -> dprint "Inst @[%t@]" Cil.d_instr i
                | `Stmt s  -> dprint "Stmt @[%t@]" Cil.d_stmt s
                | `Block b -> dprint "Block @[%t@]" Cil.d_block b
                | `Fun f   -> dprint "Fun @[%t@]" Cil.d_global (Cil.GFun (f, Cil.locUnknown))
                | _ -> super#printer ff (* workaround for bug in type checker *)
    end

class ['tree] patchnode ?(mass=0.0) data =
    object (self : 's)
        inherit ['tree] Dtree.node as super
        val tree = Some (new patchtree mass data)
    end


(* filters to hide/show patches *)
let astfilter x = match x#data with
    | `Patchmaker _ -> Domz.Reject
    | _ -> Domz.Accept

let patchfilter x = match x#parentNode#data, x#data with
    | `Patchmaker _, _ -> Domz.Accept
    | _, `Patchmaker _ -> Domz.Skip
    | _, _ -> Domz.Reject


class patcher ?(patchmakers=(List.map (fun pentry -> (pentry.patchmakerFactory (), pentry.patchmakerWeight)) !allPatchmakers))
              ?(filters=[])
              ?(patchTree=(new patchnode (`Top)))
              (target : Cil.fundec) =
    (* patcher is internally implemented as a cluster of several objects:
       - cilFilter filters out some Cil-introduced temporary variables and other structures
         that should not be modified
       - preparePatchVisitor is invoked initially (with cilFilter and other filters) to discover
         all possible patches to the input function
       - applyPatchVisitor applies a single patch taken from the list of discovered patches,
         optionally replacing the patch in the patch list(s)
    *)

    (* annoyingly, cilVisitor's output interface is not uniform across different node types;
       so we'll have to provide wrappers around patchmakingVisitor's output *)
    let id x = x in
    let li x = [ x ] in

    (* sort patchmakers by name *)
    let patchmakers = List.sort (fun (p1, _) (p2, _) -> compare p1#name p2#name) patchmakers in

    (* compound filter *)
    let filters = cilFilter::filters in
    let filterUsing ff x = not (List.exists (fun f -> not (ff f x)) filters) in

    (* locate and count all possible patches *)
    let preparePatchesVisitor patchCount patchmakers patchTree =
        object (self)
            inherit Cil.nopCilVisitor
            val mutable patchCount = patchCount
            val mutable patchTree = patchTree

            method patchCount = patchCount
            method patchTree = patchTree

            method private preparePatchesUsing : 'a 'c . (patchmakingFilter -> 'a -> bool)
                                                         -> (patchmakingVisitor -> 'a -> 'a list)
                                                         -> ('a -> 'b)
                                                         -> ('a -> 'c)
                                                         -> 'a -> 'c Cil.visitAction =
                fun ff fp fn wrap x -> (* ff: filter thunk, fp: patchmaker thunk, fn: patchnode thunk *)
                    if filterUsing ff x then begin
                        (* run one patchmaker *)
                        let makePatches astnode (pm, w) =
                            let patchmakerNode, count =
                                let patchmakerNode = (new patchnode (`Patchmaker pm))#setWeight w in
                                (* check and count useful patches *)
                                List.fold_left begin fun (pn, c) x' ->
                                    (* avoid idempotent patches *)
                                    if x' <> x then (pn#appendChild (new patchnode ~mass:1.0 (fn x')), c + 1)
                                    else (pn, c)
                                end (patchmakerNode, 0) (fp pm x)
                            in
                            (* don't append if empty *)
                            if count > 0 then begin
                                patchCount <- patchCount + count;
                                astnode#appendChild patchmakerNode
                            end else
                                astnode
                        in
                        (* run all patchmakers on current AST node *)
                        let astnode = new patchnode (fn x) in
                        let patches = List.fold_left makePatches astnode patchmakers in
                        (* append patches and recurse into children *)
                        patchTree <- (patchTree#appendChild patches)#recentChild;
                        let revert x' = patchTree <- patchTree#parentNode; x' in
                        Cil.ChangeDoChildrenPost (wrap x, revert)
                    end else
                        (* filtered *)
                        Cil.SkipChildren
            method vvdec  = self#preparePatchesUsing (fun p -> p#vvdec)  (fun p -> p#vvdec)  (fun v -> `Decl v)  id
            method vvrbl  = self#preparePatchesUsing (fun p -> p#vvrbl)  (fun p -> p#vvrbl)  (fun v -> `Var v)   id
            method vexpr  = self#preparePatchesUsing (fun p -> p#vexpr)  (fun p -> p#vexpr)  (fun e -> `Expr e)  id
            method vlval  = self#preparePatchesUsing (fun p -> p#vlval)  (fun p -> p#vlval)  (fun l -> `Lval l)  id
            method vinst  = self#preparePatchesUsing (fun p -> p#vinst)  (fun p -> p#vinst)  (fun i -> `Inst i)  li
            method vstmt  = self#preparePatchesUsing (fun p -> p#vstmt)  (fun p -> p#vstmt)  (fun s -> `Stmt s)  id
            method vblock = self#preparePatchesUsing (fun p -> p#vblock) (fun p -> p#vblock) (fun b -> `Block b) id
            method vfunc  = self#preparePatchesUsing (fun p -> p#vfunc)  (fun p -> p#vfunc)  (fun f -> `Fun f)   id
        end
    in
    let preparer = preparePatchesVisitor 0 patchmakers patchTree in
    let _ = Cil.visitCilFunction (preparer :> Cil.cilVisitor) target in


    object (self)
        val patchmakers = patchmakers
        val filters = filters
        val target = target

        (* patch tree *)
        val patchCount = preparer#patchCount
        val patchTree = preparer#patchTree

        method remaining = patchCount
        method target = target

        method take ?(p=Random.float 1.0) () =
            let n = patchTree#findN p in
            let (patcher, patched) = self#takeN ~n:n () in
            (patcher, patched, n)

        method takeN ?(n=Random.int patchCount) () =
            if n < 0 || n >= patchCount then raise Not_found;
            let applier = self#applyPatchVisitor n in
            let patched = Cil.visitCilFunction (applier :> Cil.cilVisitor) target in
            Cfg.clearCFGinfo patched;
            ignore (Cfg.cfgFun patched);
            ({< patchCount = patchCount - 1; patchTree = applier#patchnode#remove#rootNode >}, patched)


        method printStats ff =
            let printStats ff patchmakers =
                (* really slow: requires n passes where n is the number of patchmakers *)
                let total = List.fold_left
                    begin fun total (pm, w) ->
                        let pmfilter x = match x#parentNode#data, x#data with
                            | `Patchmaker pm', _ when pm' = pm -> Domz.Accept
                            | _, _ -> Domz.Skip
                        in
                        let pmWalker = new Domz.treeWalker ~filter:pmfilter patchTree in
                        let rec count total pmWalker =
                            match (try Some pmWalker#nextNode with Not_found -> None) with
                                | Some next -> count (total + 1) next (* tail-recursive *)
                                | None -> total
                        in
                        let pmTotal = count 0 pmWalker in
                        Format.fprintf ff "@[%s@ (weight %.2f): %d@]@\n" pm#name w pmTotal;
                        total + pmTotal
                    end 0 patchmakers
                in
                Format.fprintf ff "@\n@[Total : %d@]@\n" total
            in
            Format.fprintf ff "@[<hov 4>Number of patches generated:@\n%a@]" printStats patchmakers

        method printPatchTree ff =
            Format.fprintf ff "%t" patchTree#printer

        (* clone while applying patch n *)
        method private applyPatchVisitor n =
            object (self)
                (* TODO: merge copyFunctionVisitor? *)
                inherit Cil.copyFunctionVisitor as super

                val mutable count = n
                val mutable skipfiltered = false
                val mutable nodeWalker = new Domz.treeWalker ~filter:astfilter patchTree#rootNode
                val mutable patchnode = None

                method patchnode = match patchnode with
                    | Some pn -> pn
                    | None -> raise Not_found

                method private applyPatchUsing  : 'a 'b . (patchmakingFilter -> 'a -> bool)
                                                          -> ('a -> 'b Cil.visitAction)
                                                          -> ('c -> 'a)
                                                          -> ('a -> 'b)
                                                          -> 'a -> 'b Cil.visitAction =
                    fun ff super fn wrap x ->
                        if count < 0 or skipfiltered then begin
                            (* already patched, just finish cloning *)
                            super x
                        end else if filterUsing ff x then begin
                            (* not yet patched (and not filtered), keep iterating through patch list(s) *)
                            nodeWalker <- nodeWalker#nextNode;
                            let patchWalker = new Domz.treeWalker ~filter:patchfilter nodeWalker#currentNode in
                            let rec findpatch patchWalker =
                                let oldcount = count in
                                if oldcount >= 0 then count <- oldcount - 1;
                                if oldcount = 0 then begin
                                    let pn = patchWalker#currentNode in
                                    patchnode <- Some pn;
                                    let patch = fn pn#data in
                                    begin match super patch with
                                        | Cil.DoChildren ->
                                            Cil.ChangeDoChildrenPost (wrap patch, id)
                                        | Cil.ChangeDoChildrenPost (_, _)
                                        | Cil.ChangeTo (_)
                                        | Cil.SkipChildren as c ->
                                            c
                                    end
                                end else
                                    findpatch patchWalker#nextNode
                            in
                            try findpatch patchWalker#nextNode
                            with Not_found -> super x
                        end else begin
                            (* filtered, just clone *)
                            skipfiltered <- true;
                            match super x with
                                | Cil.DoChildren ->
                                    Cil.ChangeDoChildrenPost (wrap x, fun y -> skipfiltered <- false; y)
                                | Cil.ChangeDoChildrenPost (pre, post) ->
                                    Cil.ChangeDoChildrenPost (pre, fun y -> skipfiltered <- false; post y)
                                | Cil.ChangeTo (_)
                                | Cil.SkipChildren as c ->
                                    (skipfiltered <- false; c)
                        end
                    method vvdec  = self#applyPatchUsing (fun p -> p#vvdec)  super#vvdec  (function `Decl  v -> v | _ -> invalid_arg "Decl")  id
                    method vvrbl  = self#applyPatchUsing (fun p -> p#vvrbl)  super#vvrbl  (function `Var   v -> v | _ -> invalid_arg "Var")   id
                    method vexpr  = self#applyPatchUsing (fun p -> p#vexpr)  super#vexpr  (function `Expr  e -> e | _ -> invalid_arg "Expr")  id
                    method vlval  = self#applyPatchUsing (fun p -> p#vlval)  super#vlval  (function `Lval  l -> l | _ -> invalid_arg "Lval")  id
                    method vinst  = self#applyPatchUsing (fun p -> p#vinst)  super#vinst  (function `Inst  i -> i | _ -> invalid_arg "Inst")  li
                    method vstmt  = self#applyPatchUsing (fun p -> p#vstmt)  super#vstmt  (function `Stmt  s -> s | _ -> invalid_arg "Stmt")  id
                    method vblock = self#applyPatchUsing (fun p -> p#vblock) super#vblock (function `Block b -> b | _ -> invalid_arg "Block") id
                    method vfunc  = self#applyPatchUsing (fun p -> p#vfunc)  super#vfunc  (function `Fun   f -> f | _ -> invalid_arg "Fun")   id
                end
    end



(* filter functions by name *)
class functionFilter fnames =
    object (self)
        inherit patchmakingFilter
        method name = "functionFilter"
        method vinst = function
            | Cil.Call(_, Cil.Lval(Cil.Var(varinfo), Cil.NoOffset), _, _) ->
                not (List.mem varinfo.Cil.vname fnames)
            | _ -> true
    end



let find_function file fname =
    let rec find_function = function
        | Cil.GFun(fundec, _)::_ when fundec.Cil.svar.Cil.vname = fname -> fundec
        | _::tl -> find_function tl
        | _ -> failwith "main function not found!"
    in
    find_function file.Cil.globals



let doit file =
    Random.self_init ();
    let printfunc ff func =
        Format.fprintf ff "@[<hov>%s@]@\n" (Pretty.sprint max_int (Cil.defaultCilPrinter#pGlobal () (Cil.GFun(func, Cil.locUnknown))))
    in
    let main_func = find_function file "main" in
    let main_patcher = new patcher ~filters:[new functionFilter ["__BREAKPT"; "__ASSUME"; "__ASSERT"; "AND"; "OR"; "NOT"]]
                                   main_func
    in
    (* show input *)
    if !opt_showinput then
        Format.printf "Input:@\n@\n%a@\n" printfunc main_func;
    if !opt_histogram > 0 then
        let hist = Array.make main_patcher#remaining 0 in
        for i = 0 to !opt_histogram - 1 do
            let (_, _, n) = main_patcher#take () in
            hist.(n) <- hist.(n) + 1
        done;
        Array.iteri (Format.printf "%3d: %d@\n") hist 
    (* show candidates *)
    else if !opt_showallN then
        let rec take n (patcher : patcher) =
            match (try Some (patcher#takeN ~n:0 ()) with Not_found -> None) with
                | Some (patcher, patched) ->
                    Format.printf "Candidate %d:@\n@\n%a@\n" n printfunc patched;
                    take (n + 1) patcher
                | None -> ()
        in
        take 0 main_patcher
    else if !opt_showall then
        let rec take n (patcher : patcher) =
            match (try Some (patcher#take ()) with Not_found -> None) with
                | Some (patcher, patched, _) ->
                    Format.printf "Candidate %d:@\n@\n%a@\n" n printfunc patched;
                    take (n + 1) patcher
                | None -> ()
        in
        take 0 main_patcher
    else if !opt_showN >= 0 then
        begin try
            let (_, patched) = (main_patcher#takeN ~n:!opt_showN ()) in
            Format.printf "Candidate %d:@\n@\n%a@\n" !opt_showN printfunc patched
        with Not_found ->
            Format.eprintf "Pick a candidate between 0 and %d.@." (main_patcher#remaining - 1);
            exit 1
        end
    else if !opt_show != nan then
        begin try
            let (_, patched, n) = (main_patcher#take ~p:!opt_show ()) in
            Format.printf "Candidate %d at %f in cumulative distribution :@\n@\n%a@\n" n !opt_show printfunc patched
        with Invalid_argument s ->
            Format.eprintf "%s@." s;
            exit 1
        end;
    if !opt_showpatchtree then
        Format.printf "%t@\n" main_patcher#printPatchTree;
    (* print statistics *)
    Format.printf "%t@\n" main_patcher#printStats



(* Cil feature *)
let feature = {
    Cil.fd_name = "randommod";
    Cil.fd_enabled = ref false;
    Cil.fd_description = "make small random changes to a program";
    Cil.fd_extraopt = [
        ("--randommod-showinput", Arg.Set(opt_showinput),
            " show parsed input program");
        ("--randommod-show", Arg.Float(fun f -> if f < 0.0 || f >= 1.0 then raise (Arg.Bad "show f must be between 0.0 <= f < 1.0"); opt_show := f),
            "<f> show the candidate modification at point 0.0 <= f < 1.0 in the cumulative distribution of modifications");
        ("--randommod-showN", Arg.Int(fun n -> if n < 0 then raise (Arg.Bad "showN n must be >= 0"); opt_showN := n),
            "<n> show the nth candidate modification (overrides above)");
        ("--randommod-showall", Arg.Set(opt_showall),
            " show all candidate modification randomly (overrides above)");
        ("--randommod-showallN", Arg.Set(opt_showallN),
            " show all candidate modification in-order (overrides above)");
        ("--randommod-histogram", Arg.Int(fun n -> if n <= 0 then raise (Arg.Bad "histogram n must be > 0"); opt_histogram := n),
            "<n> show a histogram of patches selected in n trials (overrides above)");
        ("--randommod-showpatchtree", Arg.Set(opt_showpatchtree),
            " show the patchtree used to generate modifications")
    ];
    Cil.fd_post_check = true;
    Cil.fd_doit = doit
}


(* patchmaker registering function, has to be after Cil.feature *)
let registerPatchmaker p =
    let pentry = { patchmakerWeight = 1.0; patchmakerFactory = p } in
    let name = (p ())#name in (* too bad there aren't class constants *)
    let flag = "--randommod-"^name^"-weight" in
    let setter f = 
        if f < 0.0 || f >= 1.0 then begin
            Format.eprintf "%s value must be between 0.0 <= f <= 1.0@." flag;
            exit 1
        end;
        pentry.patchmakerWeight <- f
    in
    let opt = (flag, Arg.Float setter, "<f> set weight for "^name^" patchmaker (0.0 <= f <= 1.0)") in
    allPatchmakers := pentry::!allPatchmakers;
    feature.Cil.fd_extraopt <- opt::feature.Cil.fd_extraopt
