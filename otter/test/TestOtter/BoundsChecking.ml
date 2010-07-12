open TestUtil.MyOUnit
open Otter
open Types

(* test helper that runs the symbolic executor on a file given a source code as a string *)
let test_bounds content ?(label=content) ?(mergePaths=false) testFn =
    label >:: bracket begin fun () ->
        let filename, fileout = Filename.open_temp_file "BoundsChecking." ".c" in
        output_string fileout content;
        close_out fileout;
        filename
    end begin fun filename ->
      (* suppress all output from the symbolic executor *)
      Executeargs.print_args.Executeargs.arg_print_nothing <- true;
      Executeargs.run_args.Executeargs.arg_merge_paths <- mergePaths;

      (* reset error flag *)
      Errormsg.hadErrors := false;
      let file = Frontc.parse filename () in
      assert_bool "Cil parse error" (not !Errormsg.hadErrors);

      (* prepare the file and run the symbolic executor *)
      Executemain.prepare_file file;
      let job = Executemain.job_for_file file ["BoundsChecking"] in
      let results = if mergePaths then
        PathMerging.init job
      else
        Driver.init job
      in

      (* Check the assertions provided by the test *)
      testFn results
    end begin fun filename ->
      Unix.unlink filename
    end

(* Baseline testing function *)
let expectedResultCounts r e a t results =
  (* count completion types *)
	assert_equal
		~printer:(fun ff (r,e,a,t) -> Format.fprintf ff "%d returned; %d exited; %d abandoned; %d truncated" r e a t)
		(r,e,a,t)
		(List.fold_left
			 (fun (r,e,a,t) -> function
						Return _ -> succ r, e, a, t
					| Exit _ -> r, succ e, a, t
					| Abandoned _ -> r, e, succ a, t
					| Truncated _ -> r, e, a, succ t)
			 (0,0,0,0)
			 results)
;;

(* Some helper functions *)

let allAssertionsPassed () = assert_string (Executedebug.get_log ())
;;
(* For tests with abandoned paths, we know the error log won't be
	 empty, so we don't need to check. However, some tests don't have
	 abandoned paths, but still fail some assertions; this function lets
	 us check in those cases. *)
let someAssertionFailed () = assert_match (function s when s <> "" -> ()) (Executedebug.get_log ())
;;

(*
 * OUnit test suite
 *)

let simple_testsuite = "Simple" >::: [
  (* These are okay, even though they are out of bounds, because they
     are just taking addresses---not actually reading or writing. *)
  test_bounds ~label:"Out-of-bounds address-of"
"int main() {
  int x[2], *p = &x[-1];
  p = &x[10];
  return 0;
}"
    (fun res -> expectedResultCounts 1 0 0 0 res; allAssertionsPassed ());

  test_bounds ~label:"Acceptable negative offset"
"int main() {
  int x[2], *p = x+1;
  p[-1] = 1;
  return 0;
}"
    (fun res -> expectedResultCounts 1 0 0 0 res; allAssertionsPassed ());

  test_bounds ~label:"Explicit buffer overrun"
"int main() {
  int x[2];
  return x[2];
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);

  test_bounds ~label:"Explicit buffer underrun"
"int main() {
  int x[2];
  return x[-1];
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);

  test_bounds ~label:"Explicit malloc overrun"
"int main() {
  char *x = malloc(2);
  return x[3];
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);

  test_bounds ~label:"Explicit malloc underrun"
"int main() {
  char *x = malloc(1);
  return x[-1];
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);

  (* This test is barely different from the next one, because we treat offsets as unsigned anyway *)
  test_bounds ~label:"Possible buffer over- or underrun"
"int main() {
  int x[2], i;
  __SYMBOLIC(&i);
  return x[i];
}"
    (fun res ->
			 expectedResultCounts 1 0 0 0 res;
			 (* Warnings printed for over- and underrun *)
			 someAssertionFailed ();
			 (* Make sure the path condition is the right length *)
			 match res with
					 [Return (_,res)] ->
						 assert_equal (List.length res.result_state.path_condition) 1
							 ~msg:"Incorrect path condition"
				 | _ -> assert false
		);

  test_bounds ~label:"Possible buffer overrun (but no possible underrun)"
"int main() {
  int x[2];
  unsigned short i;
  __SYMBOLIC(&i);
  return x[i];
}"
    (fun res ->
			 expectedResultCounts 1 0 0 0 res;
			 (* Warnings printed for over- and underrun *)
			 someAssertionFailed ();
			 (* Make sure the path condition is the right length *)
			 match res with
					 [Return (_,res)] ->
						 assert_equal (List.length res.result_state.path_condition) 1
							 ~msg:"Incorrect path condition"
				 | _ -> assert false
		);

	test_bounds ~label:"Symbolic offset with no problem"
"int main() {
  int x[2];
	char *p = x + (__SYMBOLIC()==0);
	*p = 9;
  return 0;
}"
		(fun res -> expectedResultCounts 1 0 0 0 res; allAssertionsPassed ());

	test_bounds ~label:"Underrun on then branch; overrun on else branch"
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x; }
  else { p = x + 3; }
	p[1] = 7;
	p[-1] = 12;
  return 0;
}"
		(fun res -> expectedResultCounts 0 0 2 0 res;
			 (* Make sure the path conditions are each length 1 *)
			 match res with
					 [Abandoned (_,_,res1); Abandoned (_,_,res2)] ->
						 assert_equal (List.length res1.result_state.path_condition) 1
							 ~msg:"Incorrect path condition";
						 assert_equal (List.length res2.result_state.path_condition) 1
							 ~msg:"Incorrect path condition"
				 | _ -> assert false
		);

	test_bounds ~label:"Merging with no problems"
		~mergePaths:true
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x + 1; }
  else { p = x + 2; }
	p[1] = 7;
	p[-1] = 12;
  return 0;
}"
		(fun res -> expectedResultCounts 1 0 0 1 res; allAssertionsPassed ());

	test_bounds ~label:"Underrun on then branch; overrun on else branch (overrun first)---with merging"
		~mergePaths:true
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x; }
  else { p = x + 3; }
	p[1] = 7;
	p[-1] = 12;
  return 0;
}"
		(fun res -> expectedResultCounts 0 0 1 1 res;
			 (* The test should fail at the second dereference, because the
			 first one can be in bounds. This is meant to check that the
			 abandoned path has the assumption that p[1] was in bounds. *)
			 match res with
					 [Abandoned (_,_,res1); Truncated _] ->
						 assert_equal (List.length res1.result_state.path_condition) 1
							 ~msg:"Incorrect path condition";
				 | _ -> assert false
		);

	test_bounds ~label:"Underrun on then branch; overrun on else branch (underrun first)---with merging"
		~mergePaths:true
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x; }
  else { p = x + 3; }
	p[-1] = 12;
	p[1] = 7;
  return 0;
}"
		(fun res -> expectedResultCounts 0 0 1 1 res;
			 (* The test should fail at the second dereference, because the
			 first one can be in bounds. This is meant to check that the
			 abandoned path has the assumption that p[-1] was in bounds. *)
			 match res with
					 [Abandoned (_,_,res1); Truncated _] ->
						 assert_equal (List.length res1.result_state.path_condition) 1
							 ~msg:"Incorrect path condition";
				 | _ -> assert false
		);

	test_bounds ~label:"Overrun on else branch; then branch is fine---with merging"
		~mergePaths:true
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x; }
  else { p = x + 3; }
  p[0] = -3;
	p[1] = 7;
	p[2] = 12;
  return 0;
}"
		(fun res -> expectedResultCounts 1 0 0 1 res;
			 (* Make sure a warning was printed for overrun *)
			 someAssertionFailed ();
			 (* And that we assumed that we are actually in the 'then' case
			 (because the 'else' had an overrun) *)
			 match res with
					 [Return (_,res1); Truncated _] ->
						 assert_equal (List.length res1.result_state.path_condition) 1
							 ~msg:"Incorrect path condition";
				 | _ -> assert false
		);

  (* The __ASSUME here fails to constrain x[i] to be in bounds because the
		 inequality check is done in signed arithmetic. If i is big enough
		 that the multiplication overflows and the product is negative,
		 the inequality is satisfied. This means that, with 32-bit ints,
		 the constraint says roughly, 'i is 0, 1, or at least 2^29'.
		 ('Roughly' because actually the top few bits are unconstrained by
		 the inequality, so there is a little more leeway here.) Clearly,
		 then, there can still be a bounds error. *)
  test_bounds ~label:"Try to assume not out of bounds, but fail because of possible overflow"
"int main() {
  int x[2], i;
  __SYMBOLIC(&i);
  __ASSUME(0 <= i, i*4 < 8);
  return x[i];
}"
		(fun res -> expectedResultCounts 1 0 0 0 res;
			 (* Make sure a warning was printed for possible overrun *)
			 someAssertionFailed ();
		);

	(* This __ASSUME works because the cast to unsigned makes the
		 inequality function in unsigned arithmetic, so it successfully
		 constrains i to be in bounds. *)
  test_bounds ~label:"Assume not out of bounds"
"int main() {
  int x[2], i;
  __SYMBOLIC(&i);
  __ASSUME(0 <= i, (unsigned)i*sizeof(x[0]) < sizeof(x));
  return x[i];
}"
		(fun res -> expectedResultCounts 1 0 0 0 res; allAssertionsPassed ());

  test_bounds ~label:"Offset in bounds but offset+length out of bounds on read"
"int main() {
  char x[2];
  int *p = (int*)x;
  return *p;
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);

  test_bounds ~label:"Offset in bounds but offset+length out of bounds on write"
"int main() {
  char x[6];
  int *p = (int*)x;
  p[1] = 0;
  return 0;
}"
    (fun res -> expectedResultCounts 0 0 1 0 res);
]

let testsuite = "BoundsChecking" >::: [
    simple_testsuite;
]
