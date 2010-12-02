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

const char *generate_string(int length) {
    char *str = malloc(length), *p = str+length-1, c;
    *p = 0;
    while (p-- > str) {
        __SYMBOLIC(&c);
        *p = c;
    }
    return str;
}

const char *generate_byte() {
    char *str = malloc(4);
    unsigned char byte;
    __SYMBOLIC(&byte);
    snprintf(str, 4, \"%%hhu\", byte);
    return str;
}

const char *generate_int() {
    return generate_byte();
}

"

let print_declarations grammar =
    GrammarTypes.N.iter
        (fun nonterm _ -> Format.printf "const char *generate_%s(void);\n" nonterm.GrammarTypes.name)
        grammar;
    Format.printf "\n"

let is_terminal_production = function [ GrammarTypes.Term _ ] -> true | _ -> false

(** Returns [Some terminal] if the production is a terminal; otherwise returns [None] *)
let terminal_from_production = function
    | [ GrammarTypes.Term terminal ] -> Some terminal
    | _ -> None

let split_out_terminal_productions productions
        : GrammarTypes.terminal list * GrammarTypes.production list =
    List.fold_left
        (fun (terminals, others) production -> match terminal_from_production production with
             | Some terminal -> (terminal :: terminals, others)
             | None -> (terminals, production :: others))
        ([], [])
        productions

(** Like List.map, but the function also takes the index of the element in the list. *)
let mapi f lst =
    snd (List.fold_left (fun (i, result) x -> (succ i, f i x :: result)) (0, []) lst)

(** Like List.iter, but the function also takes the index of the element in the list. *)
let iteri f lst =
    ignore (List.fold_left (fun i x -> f i x; succ i) 0 lst)

let is_terminal = function GrammarTypes.Term _ -> true | GrammarTypes.Nonterm _ -> false

let generate_nonterminals nonterminals =
    iteri
        (fun i { GrammarTypes.name = name } -> Format.printf "        temp%d = generate_%s();\n" i name)
        nonterminals

let print_terminal { GrammarTypes.text = text } = Format.printf "%s" text

let malloc_result terminals nonterminals =
    Format.printf "        result = malloc(";
    iteri (fun i _ -> Format.printf "strlen(temp%d) + " i) nonterminals;
    Format.printf "sizeof(\"\""; (* Start with "" so that if there are no terminals, we get sizeof(""), which is 1, rather than sizeof(), which is a syntax error *)
    List.iter print_terminal terminals;
    Format.printf "));\n"

let write_result production =
    Format.printf "        concat(result, ";
    ignore (List.fold_left
                (fun nonterminal_index next ->
                     (match next with
                          | GrammarTypes.Term terminal -> print_terminal terminal; Format.printf ", "
                          | GrammarTypes.Nonterm _ -> Format.printf "temp%d, " nonterminal_index);
                     if is_terminal next then nonterminal_index else succ nonterminal_index)
                0
                production);
    Format.printf "0);\n"

let free_temps nonterminals =
    iteri (fun i _ -> Format.printf "        free(temp%d);\n" i) nonterminals

let split_out_terminals production
        : GrammarTypes.terminal list * GrammarTypes.nonterminal list =
    List.fold_left
        (fun (terminals, nonterminals) next -> match next with
             | GrammarTypes.Term terminal -> (terminal :: terminals, nonterminals)
             | GrammarTypes.Nonterm non -> (terminals, non :: nonterminals))
        ([], [])
        (List.rev production)

let print_common production =
    let terminals, nonterminals = split_out_terminals production in
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

(*
let print_terminals terminals =
    Format.printf "    default: result = strdup(\n";
    iteri (fun { GrammarTypes.text = text } ->
             Format.printf 
*)

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
    (* TODO: split out terminal productions from those with nonterminals, like the ftp grammar_based_client *)
    let default, rest = match productions with
        | [] -> failwith ("No productions for " ^ name)
        | hd :: tl -> hd, tl
    in
    iteri (fun n prod -> print_production n prod) rest;
    print_default default;
    Format.printf
"    return result;
}

"

let print_definitions grammar =
    GrammarTypes.N.iter print_definition grammar

let _ =
    let grammar = parse_grammar_from_in_chan stdin in
    print_prelude ();
    print_declarations grammar;
    print_definitions grammar

(*    
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

let _ = GrammarTypes.N.iter
    (fun nonterm productions ->
         print_nonterm nonterm;
         Format.printf " ::=\n";
         List.iter print_production productions)
    (main ())
*)
