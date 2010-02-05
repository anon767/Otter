(* I got the functions get_triplet, put_triplet, encodeTriplet (which
	 was called uu_encode_triplet), and build_byte_triplet from
	 http://user.it.uu.se/~pergu/uuencode.ml and uudecode.ml. I had to
	 fix put_triplet, though, by changing the two occurrences of [lsl]
	 to [lsr]. (I got the files on January 14, 2010.) *)

(* uuencoding encodes 8-bit values as a sequence of 6-bit values. This
	 essentially rewrites a string in a 256-letter alphabet into a
	 string in a 64-letter alphabet. This conversion, of course, leads
	 to an increase of the number of letters in a string. In general, to
	 go from an m-letter alphabet to an n-letter alphabet, where m > n,
	 the length increase is at least log(m)/log(n). (Since this is a
	 ratio of logs, the log's base doesn't matter.) The increase due to
	 uuencoding, therefore, is log(256)/log(64) = 4/3.

	 However, there are 95 printable ASCII characters, '\032' through
	 '\126', so we could use up to 95 characters in our output alphabet
	 and still have a printable output string (assuming the goal is to
	 take an arbitrary string and convert it, unambiguously, into a
	 printable string). Thus, the best we could do is log(256)/log(95),
	 which is about 1.2177. I think this is an irrational number (maybe
	 it's even transcendental), so I don't think you can actually
	 achieve this. I believe you can get as close as you like, but
	 getting closer means converting longer and longer substrings at a
	 time. For example, here are the first few compression rates you
	 could get, if you convert a-letter substrings of the input to
	 b-letter substrings of the output:

   a  b  compression rate
	 2  3  1.5
	 3  4  1.33333333333
	 4  5  1.25
	 9  11 1.22222222222
	 32 39 1.21875
	 55 67 1.21818181818
	 78 95 1.21794871795

	 There are clearly diminishing returns here. For someone who's happy
	 enough with a 33% increase, they can stick with uuencoding.
	 uuencoding also has the nice property that doing the encoding
	 doesn't require arithmetic, just bit-operations. However, the drop
	 to a 25% increase might be worth it; to get it, it turns out you
	 need at least 85 characters (log(256)/log(85) is about 1.248). If
	 you want the 22% increase, you need 94 (log(256)/log(94) is about
	 1.22). I like that you only need 94 (and not 95), because it means
	 you can leave a single character as your 'end of string' character.
*)

(* Return an int representing the three characters starting at str.[off] *)
let get_triplet str off =
  ((int_of_char (str.[off])) lsl 16) lor
		((int_of_char (str.[off+1])) lsl 8) lor
		(int_of_char (str.[off+2]))
;;

(* Encode an integer (which must fit on 3 bytes) as its 4-character uuencoding. *)
let encodeTriplet triplet =
	if triplet < 0 || triplet >= 1 lsl 24 then invalid_arg "encode_triplet's argument is not in [0,2^24)";
	let s = String.create 4 in
	s.[0] <- char_of_int(((triplet lsr 18) land 63)+32);
	s.[1] <- char_of_int(((triplet lsr 12) land 63)+32);
	s.[2] <- char_of_int(((triplet lsr 6) land 63)+32);
	s.[3] <- char_of_int((triplet land 63)+32);
	s
;;

(** Use uuencode-style encoding to encode any string to a string consisting only
	of bytes between 32 (' ') and 95 ('_'). The output string is terminated by
	'~'s such that the output string's length is a multiple of 4. Thus, the
	output string's length will be 4 + 4*(inputLen/3). *)
let stringTerminator = '~'
let uuencode str =
	let inLen = String.length str in
	let outLen = 4 + 4 * (inLen / 3) in
	let result = String.create outLen in
	for i = 0 to pred (inLen/3) do
		let triplet = get_triplet str (i*3) in
		let encoded = encodeTriplet triplet in
		String.blit encoded 0 result (i*4) 4
	done;
	(match inLen mod 3 with
			 0 -> (* Pad with 4 stringTerminators. *)
				 String.fill result (outLen - 4) 4 stringTerminator
		 | 1 -> (* Encode final character and pad with two stringTerminators *)
				 let triplet = (int_of_char (str.[pred inLen])) lsl 16 in
				 let encoded = encodeTriplet triplet in
				 String.blit encoded 0 result (outLen - 4) 2;
				 result.[pred outLen] <- stringTerminator;
				 result.[outLen - 2] <- stringTerminator
		 | _ (* 2 *) -> (* Encode final two characters and pad with one stringTerminator *)
				 let triplet =
					 (int_of_char (str.[inLen - 2]) lsl 16) lor
						 (int_of_char (str.[pred inLen]) lsl 8)
				 in
				 let encoded = encodeTriplet triplet in
				 String.blit encoded 0 result (outLen - 4) 3;
				 result.[pred outLen] <- stringTerminator
	);
	result
;;

(* Return the triplet (that is, the integer in [0,2^24)) represented by the four
	 characters starting at instr.[index]. These characters must all be in
	 {'\032',...,'\095'}. *)
let build_byte_triplet instr index =
  let a = int_of_char instr.[index]
  and b = int_of_char instr.[index+1]
  and c = int_of_char instr.[index+2]
  and d = int_of_char instr.[index+3]
  in
	if not (32 <= a && a <= 95 && 32 <= b && b <= 95 && 32 <= c && c <= 95 && 32 <= d && d <= 95)
	then invalid_arg ("Some character in \"" ^ (String.sub instr index 4) ^ "\" is not in [32,95].");
  ((a-32) lsl 18) lor ((b-32) lsl 12)  lor ((c-32) lsl 6) lor (d-32)
;;

(* [put_triplet str idx n] writes the three characters encoded by n
		(which must be in [0,2^24)) to str.[idx], str.[index+1], and
		str.[idx+2]. *)
let put_triplet str index triplet =
	if triplet < 0 || triplet >= 1 lsl 24 then invalid_arg "put_triplet's argument is not in [0,2^24)";
  str.[index] <- char_of_int ((triplet lsr 16) land 255);
  str.[index+1] <- char_of_int ((triplet lsr 8) land 255);
  str.[index+2] <- char_of_int (triplet land 255)
;;

(** Convert a string produced by uuencode (or, any string whose length
	is a multiple of 4 and all of whose characters are in the range
	[32,95]) to its un-uuencoded form. *)
let uudecode str =
	let inLen = String.length str in
	if inLen mod 4 <> 0
	then invalid_arg "uncompress got a string whose length is not a multiple of 4";
	let offBy =
		(* How many characters shorter than (inLen/4)*3 should the result be? This
			 depends on how many stringTerminators there are. *)
		if str.[inLen - 4] = stringTerminator
		then 3
		else if str.[inLen - 2] = stringTerminator
		then 2
		else 1
	in
	let outLen = (inLen/4)*3 - offBy in
	let result = String.create outLen in
	for i = 0 to (inLen/4) - 2 do
		put_triplet result (i*3) (build_byte_triplet str (i*4))
	done;
	if offBy = 1
	then ( (* There is one stringTerminator, so we have two more characters to decode *)
		let a = int_of_char str.[inLen-4]
		and b = int_of_char str.[inLen-3]
		and c = int_of_char str.[inLen-2]
		in
		if not (32 <= a && a <= 95 && 32 <= b && b <= 95 && 32 <= c && c <= 95)
		then invalid_arg ("Some character in \"" ^ (String.sub str (inLen-4) 3) ^ "\" is not in [32,95].");
		let triplet = ((a-32) lsl 18) lor ((b-32) lsl 12) lor (((c-32) lsl 6)) in
		let s = String.create 3 in
		put_triplet s 0 triplet;
		result.[outLen - 2] <- s.[0];
		result.[pred outLen] <- s.[1]
	) else if offBy = 2
	then ( (* There are two stringTerminators, so we have one more character to decode *)
		let a = int_of_char str.[inLen-4]
		and b = int_of_char str.[inLen-3]
		in
		if not (32 <= a && a <= 95 && 32 <= b && b <= 95)
		then invalid_arg ("Some character in \"" ^ (String.sub str (inLen-3) 2) ^ "\" is not in [32,95].");
		let triplet = ((a-32) lsl 18) lor ((b-32) lsl 12) in
		let s = String.create 3 in
		put_triplet s 0 triplet;
		result.[pred outLen] <- s.[0]
	);
	(* There are 4 stringTerminators, so we're already done. *)
	result
;;
