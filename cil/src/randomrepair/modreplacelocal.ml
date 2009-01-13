
(** Replace one variable use with another local variable. Only arithmetic types are currently replaced. *)

class patchmaker () =
    object (self)
        inherit Randommod.patchmakingVisitor
        method name = "replaceLocal"
        (* TODO: support other types? *)
        val varlist = new Ilist.c
        method private filterVars v = not (Randommod.isVarCilTemp v) && Cil.isArithmeticType v.Cil.vtype
        method vvrbl v =
            if self#filterVars v then begin
                let rec makePatches a i = try
                    let hd = i#next in
                        makePatches (hd::a) i
                    with
                        | Not_found -> a
                in
                makePatches [] varlist#iterator
            end else
                []
        method vfunc f =
            varlist#clear;
            varlist#appendList (List.filter self#filterVars f.Cil.sformals);
            varlist#appendList (List.filter self#filterVars f.Cil.slocals);
            []

    end;;
Randommod.registerPatchmaker new patchmaker
