val stringTerminator : char

(** Use uuencode-style encoding to encode any string to a string consisting only
    of bytes between 32 (' ') and 95 ('_'). The output string is padded with
    '~'s such that the output string's length is a multiple of 4. Thus, the
    output string's length will be 4 + 4*(inputLen/3), where the division is
    integer division. *)
val uuencode : string -> string

(** Convert a string produced by uuencode (or, any string whose length
    is a multiple of 4 and all of whose characters are in the range
    [32,95]) to its un-uuencoded form. *)
val uudecode : string -> string
