let parse_grammar_from_in_chan in_chan =
    let lexbuf = Lexing.from_channel in_chan in
    Grammar.input GrammarLexer.token lexbuf

let print_prelude () = Format.printf
"#include <stdarg.h>
#include <string.h>

/** Concatenate all of the arguments after the first, and store the result in the first argument. */
void concat(char *dst, const char *string, ...) {
    va_list args;
    va_start(args, string);
    const char *next = string;
    while (next) {
        dst = stpcpy(dst, next);
        next = va_arg(args, const char *);
    }
    va_end(args);
}

#define GEN_STRING(n) \\
const char *generate_string##n() {\\
    char *str = malloc((n) + 1), *p = str + (n), c;\\
    *p = 0;\\
    while (p-- > str) {\\
        __SYMBOLIC(&c);\\
        *p = c;\\
    }\\
    return str;\\
}
GEN_STRING(1)
GEN_STRING(2)
GEN_STRING(3)
GEN_STRING(4)
GEN_STRING(5)
GEN_STRING(6)
GEN_STRING(7)
GEN_STRING(8)
GEN_STRING(9)
GEN_STRING(10)

const char *generate_digit() {
    char *result = malloc(2);
    unsigned char byte;
    __SYMBOLIC(&byte);
    __ASSUME(byte >= '0');
    __ASSUME(byte <= '9');
    result[0] = byte;
    result[1] = 0;
    return result;
}

const char *generate_letter() {
    char *result = malloc(2);
    unsigned char byte;
    __SYMBOLIC(&byte);
    __ASSUME(byte >= 'A');
    __ASSUME(byte <= 'z');
    __ASSUME(OR(byte <= 'Z', byte >= 'a'));
    result[0] = byte;
    result[1] = 0;
    return result;
}

"

let print_declarations grammar =
    GrammarTypes.N.iter
        (fun nonterm _ -> Format.printf "const char *generate_%s(void);\n" nonterm.GrammarTypes.name)
        grammar;
    Format.printf "\n"

(** Like List.iter, but the function also takes the index of the element in the list. *)
let iteri f lst =
    ignore (List.fold_left (fun i x -> f i x; succ i) 0 lst)

let is_terminal = function GrammarTypes.Term _ -> true | GrammarTypes.Nonterm _ -> false

let is_single_terminal = function [x] -> is_terminal x | _ -> false

let generate_nonterminals nonterminals =
    iteri
        (fun i { GrammarTypes.name = name } -> Format.printf "        temp%d = generate_%s();\n" i name)
        nonterminals

let print_terminal { GrammarTypes.text = text } = Format.printf "\"%s\"" text

let malloc_result terminals nonterminals =
    Format.printf "        result = malloc(";
    iteri (fun i _ -> Format.printf "strlen(temp%d) + " i) nonterminals;
    (* Concatenate all terminals to find their total length *)
    Format.printf "sizeof(\"";
    List.iter (fun { GrammarTypes.text = text } -> Format.printf "%s" text) terminals;
    Format.printf "\"));\n"

let write_result production =
    Format.printf "        concat(result, ";
    ignore (List.fold_left
                (fun nonterminal_index next ->
                     (match next with
                          | GrammarTypes.Term terminal -> print_terminal terminal; Format.printf ", "
                          | GrammarTypes.Nonterm _ -> Format.printf "temp%d, " nonterminal_index);
                     if is_terminal next then nonterminal_index else succ nonterminal_index) (* Increment the index of temporary variables only when reaching a nonterminal *)
                0
                production);
    Format.printf "0);\n"

let free_temps nonterminals =
    iteri (fun i _ -> Format.printf "        free(temp%d);\n" i) nonterminals

let print_common production =
    let terminals, nonterminals = List.partition is_terminal production in
    let terminals = List.map (function GrammarTypes.Term t -> t | _ -> assert false) terminals
    and nonterminals = List.map (function GrammarTypes.Nonterm n -> n | _ -> assert false) nonterminals in
    generate_nonterminals nonterminals;
    malloc_result terminals nonterminals;
    write_result production;
    free_temps nonterminals

let print_production n production =
    Format.printf "    if (choice == %d) {\n" n;
    print_common production;
    Format.printf "    } else "

let print_default production =
    Format.printf "    {\n";
    print_common production;
    Format.printf "    }\n"

let always_fork = ref false
(** If true, don't use '?:' to keep terminals merged together in an
   if-then-else value. Instead, use if-then-else statements, forcing
   execution to fork. *)

let print_terminal_productions start_n terminal terminals =
    if !always_fork then (
        iteri (fun n prod -> print_production (n + start_n) prod)
            (List.map (fun t -> [GrammarTypes.Term t]) terminals);
        print_default [GrammarTypes.Term terminal]
    ) else (
        Format.printf "    {\n        result = strdup(\n";
        iteri (fun n terminal ->
                   Format.printf "            choice == %d ? " (n + start_n);
                   print_terminal terminal;
                   Format.printf " :\n")
            terminals;
        Format.printf "            ";
        print_terminal terminal;
        Format.printf ");\n    }\n"
    )

(** [count p list] returns how many elements of list satisfy predicate p *)
let count p list =
    List.fold_left
        (fun count x -> if p x then succ count else count)
        0
        list

let max_num_nonterminals productions =
    List.fold_left
        (fun max_so_far production ->
             max max_so_far (count (fun x -> not (is_terminal x)) production))
        (-1)
        productions

let print_cases productions =
    let terminals, others = List.partition is_single_terminal productions in
    let terminals = List.map (function [ GrammarTypes.Term t ] -> t | _ -> assert false) terminals in
    match terminals with
        | terminal :: terminals ->
              iteri (fun n prod -> print_production n prod) others;
              print_terminal_productions (List.length others) terminal terminals
        | [] -> match others with
              | [] -> failwith "No productions"
              | hd :: tl ->
                    iteri (fun n prod -> print_production n prod) tl;
                    print_default hd

let print_definition { GrammarTypes.name = name } productions =
    Format.printf "const char *generate_%s(void) {\n" name;
    Format.printf "    char *result";
    (* Declare all needed temporary variables *)
    for i = 0 to max_num_nonterminals productions - 1 do
        Format.printf ", *temp%d" i
    done;
    Format.printf ";
    int choice;
    __SYMBOLIC(&choice);
";
    print_cases productions;
    Format.printf "    return result;
}

"

let print_definitions grammar =
    GrammarTypes.N.iter print_definition grammar

let speclist = [
	("--always-fork",
	 Arg.Set always_fork,
	 " Don't use '?:' to keep terminals merged together in an if-then-else value. Instead, use if-then-else statements, forcing execution to fork.");
]

let usageMsg =
	"Usage: grammar takes its input, a file specifying a context-free grammar, on stdin, for example:\n  ./grammar < file\n"

let print_grammar_generation_code grammar =
    print_prelude ();
    print_declarations grammar;
    print_definitions grammar

let main () =
	  Arg.parse
		    (Arg.align speclist)
		    (fun _ -> raise (Arg.Help usageMsg))
		    usageMsg;
    let grammar = parse_grammar_from_in_chan stdin in
    (* TODO: Consider simplifying the grammar, for example by inlining nonterminals with only terminal productions. *)
    print_grammar_generation_code grammar
;;

main ()

(* (* What follows is code for printing out the grammar in human-readable form. *)

let print_nonterm { GrammarTypes.name = str } = Format.printf "%s" str
let print_term { GrammarTypes.text = str } = Format.printf "\"%s\"" (String.escaped str)

let print_production prod =
    Format.printf "  |";
    List.iter
        (fun x ->
             Format.printf " ";
             match x with
                   GrammarTypes.Term term -> print_term term
                 | GrammarTypes.Nonterm nonterm -> print_nonterm nonterm)
        prod;
    Format.printf "\n"

let print_grammar grammar =
    GrammarTypes.N.iter
        (fun nonterm productions ->
             print_nonterm nonterm;
             Format.printf " ::=\n";
             List.iter print_production productions)
        grammar

*)
