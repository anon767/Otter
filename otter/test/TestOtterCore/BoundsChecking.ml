open TestUtil.MyOUnit
open TestUtil.OtterUtil
open OtterCore
open State
open Job

(* test helper that runs the symbolic executor on a file given a source code as a string *)
let test_bounds content ?label test = test_otter content ?label test


(* Baseline testing function *)
let expectedResultCounts r e a results =
    (* count completion types *)
    let counts = List.fold_left begin fun (r,e,a) -> function
        | Return _ -> (succ r, e, a)
        | Exit _ -> (r, succ e, a)
        | Abandoned _ -> (r, e, succ a)
        | Truncated _ -> (r, e, a) (* ignored *)
    end (0, 0, 0) results in
    assert_equal
        ~printer:(fun ff (r,e,a) -> Format.fprintf ff "%d returned; %d exited; %d abandoned" r e a)
        (r, e, a)
        counts

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
	(fun res -> expectedResultCounts 1 0 0 res);

  test_bounds ~label:"Acceptable negative offset"
"int main() {
  int x[2], *p = x+1;
  p[-1] = 1;
  return 0;
}"
	(fun res -> expectedResultCounts 1 0 0 res);

  test_bounds ~label:"Explicit buffer overrun"
"int main() {
  int x[2];
  return x[2];
}"
	(fun res -> expectedResultCounts 0 0 1 res);

  test_bounds ~label:"Explicit buffer underrun"
"int main() {
  int x[2];
  return x[-1];
}"
	(fun res -> expectedResultCounts 0 0 1 res);

  test_bounds ~label:"Explicit malloc overrun"
"int main() {
  char *x = malloc(2);
  return x[3];
}"
	(fun res -> expectedResultCounts 0 0 1 res);

  test_bounds ~label:"Explicit malloc underrun"
"int main() {
  char *x = malloc(1);
  return x[-1];
}"
	(fun res -> expectedResultCounts 0 0 1 res);

  (* This test is barely different from the next one, because we treat offsets as unsigned anyway *)
  test_bounds ~label:"Possible buffer over- or underrun"
"int main() {
  int x[2], i;
  __SYMBOLIC(&i);
  return x[i];
}"
	(fun res -> expectedResultCounts 1 0 1 res);

  test_bounds ~label:"Possible buffer overrun (but no possible underrun)"
"int main() {
  int x[2];
  unsigned short i;
  __SYMBOLIC(&i);
  return x[i];
}"
	(fun res -> expectedResultCounts 1 0 1 res);

	test_bounds ~label:"Symbolic offset with no problem"
"int main() {
  int x[2];
	char *p = x + (__SYMBOLIC()==0);
	*p = 9;
  return 0;
}"
	(fun res -> expectedResultCounts 1 0 0 res);

	test_bounds ~label:"Underrun on then branch; overrun on else branch"
"int main() {
  char *x = malloc(4), *p;
  if (__SYMBOLIC()) { p = x; }
  else { p = x + 3; }
	p[1] = 7;
	p[-1] = 12;
  return 0;
}"
	(fun res ->
			 expectedResultCounts 0 0 2 res;
			 (* Make sure the path conditions are each length 1 *)
			 match res with
					 [Abandoned (_,_,res1); Abandoned (_,_,res2)] ->
						 assert_equal 1 (List.length res1.result_state.path_condition)
							 ~msg:"Incorrect path condition";
						 assert_equal 1 (List.length res2.result_state.path_condition)
							 ~msg:"Incorrect path condition"
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
	(fun res -> expectedResultCounts 1 0 1 res);

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
	(fun res -> expectedResultCounts 1 0 0 res);

  test_bounds ~label:"Offset in bounds but offset+length out of bounds on read"
"int main() {
  char x[2];
  int *p = (int*)x;
  return *p;
}"
	(fun res -> expectedResultCounts 0 0 1 res);

  test_bounds ~label:"Offset in bounds but offset+length out of bounds on write"
"int main() {
  char x[6];
  int *p = (int*)x;
  p[1] = 0;
  return 0;
}"
	(fun res -> expectedResultCounts 0 0 1 res);
]

let testsuite = "BoundsChecking" >::: [
    simple_testsuite;
]
