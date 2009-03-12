(* CFL reachability *)
module Check
    (G : sig
        type t
        type cfl
        module V : Sig.COMPARABLE
        val start_cfl : cfl
        val accept_cfl : cfl -> bool
        val iter_succ_cfl : (V.t -> cfl -> unit) -> t -> V.t -> cfl -> unit
    end) = struct

    module HV = Hashtbl.Make(G.V)
    module HVV = Hashtbl.Make(Util.HTProduct(G.V)(G.V))

    (* the cache contains the path tests already computed *)
    type path_checker = { cache : bool HVV.t; graph : G.t }

    let create g = { cache = HVV.create 97; graph = g }

    let check_path pc v1 v2 = try
        HVV.find pc.cache (v1, v2)
    with Not_found ->
        (* the path is not in cache; we check it with Dijkstra *)
        let visited = HV.create 97 in
        let q = Queue.create () in
        let rec loop () =
            if Queue.is_empty q then begin
                HVV.add pc.cache (v1, v2) false;
                false
            end else begin
                let (v, cfl) = Queue.pop q in
                let accept = G.accept_cfl cfl in
                HVV.add pc.cache (v1, v) accept;
                if accept && G.V.compare v v2 = 0 then
                    true
                else begin
                    if not (HV.mem visited v) then begin
                        HV.add visited v ();
                        G.iter_succ_cfl (fun v' cfl -> Queue.add (v', cfl) q) pc.graph v cfl
                    end;
                    loop ()
                end
            end
        in
        Queue.add (v1, G.start_cfl) q;
        loop ()
end
