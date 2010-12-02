%{
%}

%token <GrammarTypes.terminal> TERM
%token <GrammarTypes.nonterminal> NONTERM
%token DEF PIPE SEMICOLON EOF

%start input
%type <GrammarTypes.production> production
%type <GrammarTypes.production list> productions
%type <GrammarTypes.grammar> input
%type <GrammarTypes.nonterminal * GrammarTypes.production list> rewrite_rule

%% /* Grammar rules and actions follow */
input:
  EOF                { GrammarTypes.N.empty }
| rewrite_rule input { let nonterm, prods = $1 in
                       let old_prods = try GrammarTypes.N.find nonterm $2 with Not_found -> [] in
                       GrammarTypes.N.add nonterm (List.rev_append prods old_prods) $2 }
;

rewrite_rule:
  NONTERM DEF productions SEMICOLON { ($1, $3) }
| NONTERM DEF PIPE productions SEMICOLON { ($1, $4) }
;

productions:
  production { [$1] }
| production PIPE productions { $1 :: $3 }
;

production:
  TERM               { [GrammarTypes.Term $1] }
| NONTERM            { [GrammarTypes.Nonterm $1] }
| TERM production    { GrammarTypes.Term $1 :: $2 }
| NONTERM production { GrammarTypes.Nonterm $1 :: $2 }
;

%%
