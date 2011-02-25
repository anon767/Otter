(** Generic memoization wrappers. *)

(* TODO: weak sets (hashconsing), weak-key maps (property map), weak-key-and-value maps (caching);
 * is there a need for weak-value maps or weak-key-or-value maps? *)

(**/**) (* helpers *)
let initial_size = 128
let statistics = Hashtbl.create 8
(**/**)

(** Make a memoization wrapper that can be used to memoized several functions of the same type *)
let make label =
    if Hashtbl.mem statistics label then FormatPlus.invalid_arg "Label %s already exists!" label;
    let memotable = Hashtbl.create initial_size in
    let hits = ref 0 in
    let misses = ref 0 in
    Hashtbl.add statistics label (hits, misses);
    fun f x ->
        try
            let y = Hashtbl.find memotable x in
            incr hits;
            y
        with Not_found ->
            let y = f x in
            (* don't need Hashtbl.replace, which is slower, as x guaranteed to be unique *)
            Hashtbl.add memotable x y;
            incr misses;
            y


(** Make a hashconsing wrapper *)
let make_hashcons label =
    (* TODO: use Weak? *)
    make label (fun x -> x)


(** Memoize a function *)
let memo label f =
    make label f


(** Memoize a recursive function; the function will be given the memoized version of itself to recurse with *)
let memo_rec label f =
    let wrap = make label in
    let rec g x = wrap (f g) x in
    g


(** Functorized memoization functions *)
module Make (T : Hashtbl.HashedType) = struct
    (* TODO: get rid of this functor somehow since it duplicates code *)
    (** Make a memoization wrapper that can be used to memoized several functions of the same type *)
    let make label =
        let module H = Hashtbl.Make (T) in
        if Hashtbl.mem statistics label then FormatPlus.invalid_arg "Label %s already exists!" label;
        let memotable = H.create initial_size in
        let hits = ref 0 in
        let misses = ref 0 in
        Hashtbl.add statistics label (hits, misses);
        fun f x ->
            try
                let y = H.find memotable x in
                incr hits;
                y
            with Not_found ->
                let y = f x in
                H.add memotable x y;
                incr misses;
                y


    (** Make a hashconsing wrapper *)
    let make_hashcons label =
        (* TODO: use Weak? *)
        make label (fun x -> x)


    (** Memoize a function *)
    let memo label f =
        make label f


    (** Memoize a recursive function; the function will be given the memoized version of itself to recurse with *)
    let memo_rec label f =
        let wrap = make label in
        let rec g x = wrap (f g) x in
        g
end


(** Print the statistics of all memoized functions *)
let statistics_printer ff =
    let table, width = Hashtbl.fold begin fun label (hits, misses) (table, width) ->
        let table = if !misses > 0 then
            (label, !hits, !misses, (float_of_int !misses /. float_of_int (!hits + !misses)))::table
        else
            table
        in
        let width = max (String.length label) width in
        (table, width)
    end statistics ([], 16) in
    let table = List.fast_sort (fun (_, _, _, x) (_, _, _, y) -> Pervasives.compare x y) table in
    let width = width + 2 in

    Format.pp_open_tbox ff ();

    Format.fprintf ff "%-*s" width "Memo statistics:";
    Format.pp_set_tab ff ();
    Format.fprintf ff "%9s %9s %9s@\n" "hits" "misses" "miss rate";

    List.iter begin fun (label, hits, misses, rate) ->
        Format.pp_print_string ff label;
        Format.pp_print_tab ff ();
        Format.fprintf ff "%9d %9d %9.5f@\n" hits misses rate;
    end table;

    Format.pp_close_tbox ff ()

