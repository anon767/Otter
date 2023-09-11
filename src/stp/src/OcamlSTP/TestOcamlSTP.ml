open OcamlSTP

let error_count = ref 0

let test_query =
    let truth_printer ff v =
        Printf.fprintf ff "%s"
            begin match v with
                | `True -> "True"
                | `False -> "False"
                | `Unknown -> "Unknown"
            end
    in
    fun label test_fn ->
        Printf.printf "%s: %!" label;
        let count = ref 1 in
        test_fn
            begin fun s x expected ->
                let actual = match vc_query s x with
                    | Valid -> `True
                    | Invalid ->
                        begin match vc_query s (bool_not s x) with
                            | Valid -> `False
                            | Invalid -> `Unknown
                            | Undecided -> `Unknown
                        end
                    | Undecided -> `Unknown
                in
                if expected = actual then
                    Printf.printf ".%!"
                else begin
                    Printf.printf "\n\t%4d: FAIL: expected %a but got %a.%!" !count truth_printer expected truth_printer actual;
                    incr error_count
                end;
                incr count
            end
            begin fun cond ->
                if cond then
                    Printf.printf ".%!"
                else begin
                    Printf.printf "\n\t%4d: FAIL: assertion%!" !count;
                    incr error_count
                end;
                incr count
            end;
        Printf.printf "\n%!"


let () =
    let start = Sys.time () in

    begin
        let s = make_context () in
        OcamlSTP.set_seed s None;
        OcamlSTP.set_seed s (Some 1);
        OcamlSTP.set_seed s (Some 101);
        let b = bool_var s "b" in
        let v = bv_var s "v" 2 in
        let a = array_var s "a" 2 2 in
        ignore b;
        ignore v;
        ignore a;
    end;


    test_query "bool_not" begin fun test_query _assert_bool ->
        let s = make_context () in
        let b = bool_var s "b" in

        test_query s (bool_not s (bool_true s)) `False;
        test_query s (bool_not s (bool_false s)) `True;
        test_query s (bool_not s b) `Unknown;
    end;


    begin
        let test_bool2 label stp_op op =
            test_query label begin fun test_query _assert_bool ->
                let s = make_context () in
                let b = bool_var s "b" in
                let of_bool s b = (if b then bool_true else bool_false) s in
                List.iter begin fun x ->
                    List.iter begin fun y ->
                        let expr = stp_op s (of_bool s x) (of_bool s y) in
                        let expected = if op x y then`True else `False in
                        test_query s expr expected;
                    end [ true; false ]
                end [ true; false ];

                List.iter begin fun x ->
                    let expr = stp_op s b (of_bool s x) in
                    let expected = if op true x == op false x then (if op true x then `True else `False) else `Unknown in
                    test_query s expr expected;

                    let expr = stp_op s (of_bool s x) b in
                    let expected = if op x true == op x false then (if op x true then `True else `False) else `Unknown in
                    test_query s expr expected;
                end [ true; false ];

                let expr = stp_op s b b in
                let expected = if op true true == op false false then (if op true true then `True else `False) else `Unknown in
                test_query s expr expected;
            end
        in
        test_bool2 "bool_and" bool_and (&&);
        test_bool2 "bool_or" bool_or (||);
        test_bool2 "bool_nand" bool_nand (fun x y -> not (x && y));
        test_bool2 "bool_nor" bool_nor (fun x y -> not (x || y));
        test_bool2 "bool_xor" bool_xor (!=);
        test_bool2 "bool_iff" bool_iff (==);
        test_bool2 "bool_implies" bool_implies (fun x y -> not x || y);
    end;


    begin
        let test_bv1 label ?(width=2) ?(range=[ 0; 1; 2; 3 ]) stp_op op =
            let mask x = x land (1 lsl width - 1) in
            let op_eq x y = mask (op x) == mask y in
            test_query label begin fun test_query _assert_bool ->
                let s = make_context () in
                let v = bv_var s "v" width in

                List.iter begin fun x ->
                    List.iter begin fun y ->
                        let expr = bv_eq s (stp_op s (bv_of_int s width x)) (bv_of_int s width y) in
                        let expected = if op_eq x y then `True else `False in
                        test_query s expr expected;
                    end range;

                    let expr = bv_eq s (stp_op s v) (bv_of_int s width x) in
                    let expected =
                        if List.for_all (fun y -> op_eq y x) range then `True
                        else if List.for_all (fun y -> not (op_eq y x)) range then `False
                        else `Unknown
                    in
                    test_query s expr expected;
                end range;

                let expr = bv_eq s (stp_op s v) v in
                let expected =
                    if List.for_all (fun x -> op_eq x x) range then `True
                    else if List.for_all (fun x -> not (op_eq x x)) range then `False
                    else `Unknown
                in
                test_query s expr expected;
            end
        in
        test_bv1 "bv_neg" bv_neg (~-);
        test_bv1 "bv_not" bv_not lnot;
    end;


    begin
        let test_bv2 label ?(width=2) ?(lrange=[ 0; 1; 2; 3 ]) ?(rrange=lrange) ?(skiprv=false) stp_op op =
            let mask x = x land (1 lsl width - 1) in
            let op_eq x y z = mask (op x y) == mask z in
            test_query label begin fun test_query _assert_bool ->
                let s = make_context () in
                let v = bv_var s "v" width in

                List.iter begin fun x ->
                    List.iter begin fun y ->
                        List.iter begin fun z ->
                            let expr = bv_eq s (stp_op s (bv_of_int s width x) (bv_of_int s width y)) (bv_of_int s width z) in
                            let expected = if op_eq x y z then`True else `False in
                            test_query s expr expected;
                        end lrange;
                    end rrange;
                end lrange;

                List.iter begin fun x ->
                    List.iter begin fun y ->
                        let expr = bv_eq s (stp_op s v (bv_of_int s width x)) (bv_of_int s width y) in
                        let expected =
                            if List.for_all (fun z -> op_eq z x y) lrange then`True
                            else if List.for_all (fun z -> not (op_eq z x y)) lrange then`False
                            else `Unknown
                        in
                        test_query s expr expected;
                    end lrange;
                end rrange;

                if not skiprv then begin
                    List.iter begin fun x ->
                        List.iter begin fun y ->
                            let expr = bv_eq s (stp_op s (bv_of_int s width x) v) (bv_of_int s width y) in
                            let expected =
                                if List.for_all (fun z -> op_eq x z y) rrange then`True
                                else if List.for_all (fun z -> not (op_eq x z y)) rrange then`False
                                else `Unknown
                            in
                            test_query s expr expected;
                        end lrange;
                    end lrange;

                    List.iter begin fun x ->
                        let expr = bv_eq s (stp_op s v v) (bv_of_int s width x) in
                        let expected =
                            if List.for_all (fun y -> op_eq y y x) rrange then`True
                            else if List.for_all (fun y -> not (op_eq y y x)) rrange then`False
                            else `Unknown
                        in
                        test_query s expr expected;
                    end lrange;

                    let expr = bv_eq s (stp_op s v v) v in
                    let expected =
                        if List.for_all (fun x -> op_eq x x x) rrange then`True
                        else if List.for_all (fun x -> not (op_eq x x x)) rrange then`False
                        else `Unknown
                    in
                    test_query s expr expected;
                end
            end
        in
        test_bv2 "bv_add" bv_add (+);
        test_bv2 "bv_sub" bv_sub (-);
        test_bv2 "bv_mul" bv_mul ( * );
        test_bv2 "bv_div" bv_div (/) ~rrange:[ 1; 2; 3 ] ~skiprv:true;
        test_bv2 "bv_rem" bv_rem (mod) ~rrange:[ 1; 2; 3 ] ~skiprv:true;
        test_bv2 "bv_signed_div" bv_signed_div (/) ~lrange:[ -2; -1; 0; 1 ] ~rrange:[ -2; -1; 1 ] ~skiprv:true;
        test_bv2 "bv_signed_rem" bv_signed_rem (mod) ~lrange:[ -2; -1; 0; 1 ] ~rrange:[ -2; -1; 1 ] ~skiprv:true;
        let smod x y =
            let z = x - y * truncate (floor (float x /. float y)) in
            if z == 0 && (x < 0) != (y < 0) then 0 else z
        in
        test_bv2 "bv_signed_mod" bv_signed_mod smod ~lrange:[ -2; -1; 0; 1 ] ~rrange:[ -2; -1; 1 ] ~skiprv:true;

        test_bv2 "bv_shift_left" bv_shift_left (lsl);
        test_bv2 "bv_shift_right" bv_shift_right (lsr);
        let srs =
            let shift = Sys.word_size - 3 in
            fun x y -> ((x lsl shift) asr y) lsr shift
        in
        test_bv2 "bv_signed_shift_right" bv_signed_shift_right srs;

        test_bv2 "bv_and" bv_and (land);
        test_bv2 "bv_or" bv_or (lor);
        test_bv2 "bv_xor" bv_xor (lxor);
(* Buggy
        test_bv2 "bv_nor" bv_nor (fun x y -> lnot (x lor y));
        test_bv2 "bv_nand" bv_nand (fun x y -> lnot (x land y));
        test_bv2 "bv_xnor" bv_xnor (fun x y -> lnot (x lxor y));
*)
    end;


    test_query "vc_push/vc_pop" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        vc_push s;
        vc_assert s (bv_gt s x (bv_of_int s 2 0));
        test_query s (bv_eq s x (bv_of_int s 2 0)) `False;
        test_query s (bv_eq s x (bv_of_int s 2 1)) `Unknown;
        vc_push s;
        vc_assert s (bv_gt s x (bv_of_int s 2 1));
        test_query s (bv_eq s x (bv_of_int s 2 1)) `False;
        test_query s (bv_eq s x (bv_of_int s 2 2)) `Unknown;
        vc_push s;
        vc_assert s (bv_gt s x (bv_of_int s 2 2));
        test_query s (bv_eq s x (bv_of_int s 2 2)) `False;
        test_query s (bv_eq s x (bv_of_int s 2 3)) `True;
        vc_pop s;
        test_query s (bv_eq s x (bv_of_int s 2 2)) `Unknown;
        test_query s (bv_eq s x (bv_of_int s 2 1)) `False;
        vc_pop s;
        test_query s (bv_eq s x (bv_of_int s 2 1)) `Unknown;
        test_query s (bv_eq s x (bv_of_int s 2 0)) `False;
        vc_pop s;
        test_query s (bv_eq s x (bv_of_int s 2 0)) `Unknown;
    end;


    test_query "bv_get_bit" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        let y = bv_of_int s 2 2 in
        let j = bv_get_bit s x 0 in
        let k = bv_get_bit s x 1 in
        vc_assert s (bv_ge s x y);
        test_query s j `Unknown;
        test_query s k `True;
    end;


    test_query "bv_concat" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        let y = bv_var s "y" 2 in
        let xy = bv_concat s x y in
        let x0 = bv_concat s x (bv_of_int s 2 0) in
        let y0 = bv_concat s y (bv_of_int s 2 0) in
        test_query s (bv_ge s xy x0) `True;
        test_query s (bv_ge s xy y0) `Unknown;
    end;


    test_query "bv_extract" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        let y = bv_var s "y" 2 in
        let z = bv_var s "z" 2 in
        let xyz = bv_concat s (bv_concat s x y) z in
        let x' = bv_extract s xyz 5 4 in
        let y' = bv_extract s xyz 3 2 in
        let z' = bv_extract s xyz 1 0 in
        test_query s (bv_eq s x x') `True;
        test_query s (bv_eq s y y') `True;
        test_query s (bv_eq s z z') `True;
        test_query s (bv_eq s x y') `Unknown;
        test_query s (bv_eq s x z') `Unknown;
        test_query s (bv_eq s y x') `Unknown;
        test_query s (bv_eq s y z') `Unknown;
        test_query s (bv_eq s z x') `Unknown;
        test_query s (bv_eq s z y') `Unknown;
        let u = bv_extract s xyz 5 5 in
        let v = bv_extract s xyz 0 0 in
        test_query s (bv_eq s (bv_extract s x' 1 1) u) `True;
        test_query s (bv_eq s (bv_extract s z' 0 0) v) `True;
    end;

    test_query "bv_sign_extend" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        let y = bv_sign_extend s x 4 in
        let z = bv_sign_extend s y 6 in
        vc_push s;
            test_query s (bv_signed_ge s y (bv_of_int s 4 0)) `Unknown;
            test_query s (bv_signed_ge s z (bv_of_int s 6 0)) `Unknown;
        vc_pop s;
        vc_push s;
            vc_assert s (bv_signed_lt s x (bv_of_int s 2 0));
            test_query s (bv_signed_lt s y (bv_of_int s 4 0)) `True;
            test_query s (bv_signed_lt s z (bv_of_int s 6 0)) `True;
        vc_pop s;
        vc_push s;
            vc_assert s (bv_signed_ge s x (bv_of_int s 2 0));
            test_query s (bv_signed_ge s y (bv_of_int s 4 0)) `True;
            test_query s (bv_signed_ge s z (bv_of_int s 6 0)) `True;
        vc_pop s;
        vc_push s;
            vc_assert s (bv_signed_lt s y (bv_of_int s 4 0));
            test_query s (bv_signed_lt s x (bv_of_int s 2 0)) `True;
            test_query s (bv_signed_lt s z (bv_of_int s 6 0)) `True;
        vc_pop s;
        vc_push s;
            vc_assert s (bv_signed_ge s y (bv_of_int s 4 0));
            test_query s (bv_signed_ge s x (bv_of_int s 2 0)) `True;
            test_query s (bv_signed_ge s z (bv_of_int s 6 0)) `True;
        vc_pop s;
    end;

    test_query "bv_zero_extend" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 2 in
        let y = bv_zero_extend s x 4 in
        let z = bv_zero_extend s y 6 in
        test_query s (bv_lt s y (bv_of_int s 4 5)) `True;
        test_query s (bv_lt s z (bv_of_int s 6 5)) `True;
        test_query s (bv_eq s y (bv_of_int s 4 0)) `Unknown;
        test_query s (bv_eq s z (bv_of_int s 6 0)) `Unknown;
        test_query s (bv_eq s y (bv_of_int s 4 15)) `False;
        test_query s (bv_eq s z (bv_of_int s 6 63)) `False;
    end;

    test_query "bv_shift_left/bv_shift_right/bv_signed_shift_right" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bv_var s "x" 4 in
        let y = bv_var s "y" 4 in
        test_query s (bv_eq s (bv_shift_left s x (bv_of_int s 4 4)) (bv_of_int s 4 0)) `True;
        test_query s (bv_eq s (bv_shift_right s x (bv_of_int s 4 4)) (bv_of_int s 4 0)) `True;
        test_query s (bv_eq s (bv_signed_shift_right s x (bv_of_int s 4 4)) (bv_of_int s 4 0)) `Unknown;
        vc_assert s (bv_eq s x (bv_of_int s 4 0b1001));
        test_query s (bv_gt s (bv_shift_left s x (bv_of_int s 4 1)) x) `False;
        test_query s (bv_lt s (bv_shift_right s x (bv_of_int s 4 1)) x) `True;
        test_query s (bv_lt s (bv_signed_shift_right s x (bv_of_int s 4 1)) x) `False;
        test_query s (bv_gt s (bv_signed_shift_right s x (bv_of_int s 4 1)) x) `True;
        test_query s (bv_signed_lt s (bv_signed_shift_right s x (bv_of_int s 4 1)) x) `False;
        vc_push s;
        vc_assert s (bv_lt s y (bv_of_int s 4 4));
        test_query s (bv_gt s (bv_shift_left s x y) x) `False;
        test_query s (bv_le s (bv_shift_right s x y) x) `True;
        test_query s (bv_lt s (bv_signed_shift_right s x y) x) `False;
        test_query s (bv_signed_ge s (bv_signed_shift_right s x y) x) `True;
        test_query s (bv_eq s (bv_shift_left s x y) (bv_of_int s 4 0)) `False;
        test_query s (bv_eq s (bv_shift_right s x y) (bv_of_int s 4 0)) `False;
        test_query s (bv_eq s (bv_signed_shift_right s x y) (bv_of_int s 4 0)) `False;
        vc_pop s;
        vc_push s;
        vc_assert s (bv_ge s y (bv_of_int s 4 4));
        test_query s (bv_gt s (bv_shift_left s x y) x) `False;
        test_query s (bv_lt s (bv_shift_right s x y) x) `True;
        test_query s (bv_le s (bv_signed_shift_right s x y) x) `False;
        test_query s (bv_signed_ge s (bv_signed_shift_right s x y) x) `True;
        test_query s (bv_eq s (bv_shift_left s x y) (bv_of_int s 4 0)) `True;
        test_query s (bv_eq s (bv_shift_right s x y) (bv_of_int s 4 0)) `True;
        test_query s (bv_eq s (bv_signed_shift_right s x y) (bv_of_int s 4 0)) `False;
        vc_pop s;
    end;


    test_query "bool_t ite" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bool_var s "x" in
        let y = bool_var s "y" in
        let z = bool_var s "z" in
        let e = ite s x y z in
        vc_assert s y;
        vc_assert s (bool_not s z);
        test_query s e `Unknown;
        vc_push s;
        vc_assert s x;
        test_query s e `True;
        vc_pop s;
        test_query s e `Unknown;
        vc_push s;
        vc_assert s (bool_not s x);
        test_query s e `False;
        vc_pop s;
        test_query s e `Unknown;
        vc_push s;
        vc_assert s e;
        test_query s x `True;
        vc_pop s;
        test_query s x `Unknown;
    end;


    test_query "bv_t ite" begin fun test_query _assert_bool ->
        let s = make_context () in
        let x = bool_var s "x" in
        let y = bv_var s "y" 2 in
        let z = bv_var s "z" 2 in
        let e = ite s x y z in
        vc_assert s (bv_eq s y (bv_of_int s 2 1));
        vc_assert s (bv_eq s z (bv_of_int s 2 2));
        test_query s (bv_eq s e (bv_of_int s 2 0)) `False;
        test_query s (bv_eq s e (bv_of_int s 2 1)) `Unknown;
        test_query s (bv_eq s e (bv_of_int s 2 2)) `Unknown;
        test_query s (bv_eq s e (bv_of_int s 2 3)) `False;
        vc_push s;
        vc_assert s x;
        test_query s (bv_eq s e (bv_of_int s 2 1)) `True;
        vc_pop s;
        vc_push s;
        vc_assert s (bool_not s x);
        test_query s (bv_eq s e (bv_of_int s 2 2)) `True;
        vc_pop s;
        vc_push s;
        vc_assert s (bv_eq s e (bv_of_int s 2 1));
        test_query s x `True;
        vc_pop s;
        vc_push s;
        vc_assert s (bv_eq s e (bv_of_int s 2 2));
        test_query s x `False;
        vc_pop s;
        test_query s x `Unknown;
    end;


    test_query "array_t ite" begin fun test_query _assert_bool ->
        let s = make_context () in
        let j = bv_var s "j" 2 in
        let k = bv_var s "k" 2 in
        let x = bool_var s "x" in
        let y =
            let y = array_var s "y" 2 2 in
            let y = array_write s y (bv_of_int s 2 0) (bv_of_int s 2 0) in
            let y = array_write s y j (bv_of_int s 2 1) in
            y
        in
        let z =
            let z = array_var s "z" 2 2 in
            let z = array_write s z (bv_of_int s 2 2) (bv_of_int s 2 2) in
            let z = array_write s z
                        (ite s (bv_get_bit s k 0) (bv_of_int s 2 2) (bv_of_int s 2 3))
                        (ite s (bv_get_bit s k 1) (bv_of_int s 2 2) (bv_of_int s 2 3))
            in
            z
        in
        let e = ite s x y z in
        test_query s (bv_eq s (array_read s e (bv_of_int s 2 0)) (bv_of_int s 2 0)) `Unknown;
        vc_push s;
        vc_assert s x;
        test_query s (bv_eq s (array_read s e j) (bv_of_int s 2 1)) `True;
        vc_push s;
        vc_assert s (bv_gt s j (bv_of_int s 2 0));
        test_query s (bv_eq s (array_read s e (bv_of_int s 2 0)) (bv_of_int s 2 0)) `True;
        vc_pop s;
        vc_push s;
        vc_assert s (bv_le s j (bv_of_int s 2 1));
        vc_assert s (bv_eq s (array_read s e (bv_of_int s 2 0)) (bv_of_int s 2 0));
        test_query s (bv_eq s (array_read s y (bv_of_int s 2 1)) (bv_of_int s 2 1)) `True;
        vc_pop s;
        vc_push s;
        vc_assert s (bv_get_bit s (array_read s e (bv_of_int s 2 0)) 1);
        test_query s (bv_eq s j (bv_of_int s 2 0)) `True;
        vc_pop s;
        vc_pop s;
        vc_push s;
        vc_assert s (bool_not s x);
        vc_push s;
        vc_assert s (bv_eq s k (bv_of_int s 2 1));
        test_query s (bv_eq s (array_read s e (bv_of_int s 2 2)) (bv_of_int s 2 3)) `True;
    end;


    test_query "multiple vc" begin fun test_query _assert_bool ->
        let s1 = make_context () in
        let x1 = bv_var s1 "x" 2 in
            let s2 = make_context () in
            let x2 = bv_var s2 "x" 3 in
            let y2 = bv_var s2 "y" 2 in
        let y1 = bool_var s1 "y" in
        vc_assert s1 (bv_eq s1 x1 (bv_of_int s1 2 0));
                let s3 = make_context () in
                let j3 = bv_var s3 "j" 2 in
                let k3 = bv_var s3 "k" 2 in
            vc_assert s2 (bv_eq s2 x2 (bv_of_int s2 3 7));
                let x3 = bool_var s3 "x" in
            let z2 = bv_extract s2 x2 2 1 in
                let y3 = array_var s3 "y" 2 2 in
                let y3 = array_write s3 y3 (bv_of_int s3 2 0) (bv_of_int s3 2 0) in
            vc_assert s2 (bv_eq s2 y2 z2);
                let y3 = array_write s3 y3 j3 (bv_of_int s3 2 1) in
        let z1 = ite s1 y1 x1 (bv_var s1 "z-else" 2) in
                let z3 = array_var s3 "z" 2 2 in
                let z3 = array_write s3 z3 (bv_of_int s3 2 2) (bv_of_int s3 2 2) in
        vc_assert s1 (bv_gt s1 z1 (bv_of_int s1 2 0));
                let z3 = array_write s3 z3
                            (ite s3 (bv_get_bit s3 k3 0) (bv_of_int s3 2 2) (bv_of_int s3 2 3))
                            (ite s3 (bv_get_bit s3 k3 1) (bv_of_int s3 2 2) (bv_of_int s3 2 3))
                in
                    let s4 = make_context () in
                let e3 = ite s3 x3 y3 z3 in
                    let x4 = bv_var s4 "x" 4 in
                test_query s3 (bv_eq s3 (array_read s3 e3 (bv_of_int s3 2 0)) (bv_of_int s3 2 0)) `Unknown;
                    let y4 = bv_var s4 "y" 4 in
                vc_assert s3 x3;
                    test_query s4 (bv_eq s4 (bv_signed_shift_right s4 x4 (bv_of_int s4 4 4)) (bv_of_int s4 4 0)) `Unknown;
                test_query s3 (bv_eq s3 (array_read s3 e3 j3) (bv_of_int s3 2 1)) `True;
                    vc_assert s4 (bv_eq s4 x4 (bv_of_int s4 4 0b1001));
            test_query s2 (bv_eq s2 y2 (bv_of_int s2 2 3)) `True;
                    test_query s4 (bv_gt s4 (bv_shift_left s4 x4 (bv_of_int s4 4 1)) x4) `False;
                vc_assert s3 (bv_get_bit s3 (array_read s3 e3 (bv_of_int s3 2 0)) 1);
                    vc_assert s4 (bv_lt s4 y4 (bv_of_int s4 4 4));
                    test_query s4 (bv_gt s4 (bv_shift_left s4 x4 y4) x4) `False;
                test_query s3 (bv_eq s3 j3 (bv_of_int s3 2 0)) `True;
        test_query s1 y1 `False;
    end;


    test_query "vc_get_asserts" begin fun _test_query assert_bool ->
        let s = make_context () in
        let x = bool_var s "x" in
        let y = bool_var s "y" in
        let z = bool_var s "z" in
        let a = vc_get_asserts s in
        assert_bool (a = []);
        vc_push s;
        vc_assert s x;
        let a = vc_get_asserts s in
        assert_bool (a = [ x ]);
        vc_push s;
        vc_assert s y;
        let a = vc_get_asserts s in
        assert_bool (a = [ x; y ]);
        vc_pop s;
        vc_push s;
        vc_assert s z;
        vc_assert s y;
        let a = vc_get_asserts s in
        assert_bool (a = [ x; z; y ]);
        vc_pop s;
        vc_pop s;
        let a = vc_get_asserts s in
        assert_bool (a = []);
    end;


    test_query "counterexample" begin fun _test_query assert_bool ->
        let s = make_context () in
        let b = bool_var s "b" in
        let v = bv_var s "v" 2 in

        vc_push s;
        vc_assert s b;
        assert_bool (vc_query s (bool_not s b) = Invalid);
        assert_bool (match vc_get_counterexample s b with Some b' -> to_bool s b' | None -> false);
        vc_pop s;

        vc_push s;
        vc_assert s (bv_gt s v (bv_of_int s 2 1));
        assert_bool (vc_query s (bv_eq s v (bv_of_int s 2 1)) = Invalid);
        assert_bool (match vc_get_counterexample s v with Some v' -> let v'' = to_int s v' in List.mem v'' [ 2; 3 ] | None -> false);
        vc_pop s;
    end;


    begin
        (* create some strings to test that they are garbage collected *)
        let s = make_context () in
        ignore (to_string s (bv_var s "a" 16));
        ignore (to_string s (bv_of_int s 32 0xabcd));
        ignore (to_string s (bv_xor s (bv_var s "p" 24) (bv_of_int s 24 0x2357)));
        ignore (to_string s (array_write s (array_var s "r" 2 2) (bv_of_int s 2 0) (bv_var s "s" 2)));
        ignore (to_string s (ite s (bool_var s "x") (array_var s "y" 2 2) (array_var s "z" 2 2)));
    end;


    Printf.printf "Elapsed %.5f seconds\n%!" (Sys.time () -. start);

    Pervasives.at_exit Gc.compact;

    if !error_count <> 0 then begin
        Printf.printf "%d errors found.\n\n%!" !error_count;
        exit 1
    end

