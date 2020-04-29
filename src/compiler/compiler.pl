% @author Aditya Bajaj
% @author Aihaab Shaikh
% @author Sakshi Jain
% @author Sukhpreet Singh Anand
% @version 1.7
% @purpose A lexer to parse the source file and generate tokens and
%          parser to consume the tokens to generate the parse tree.
% @date 04/25/2020

%------------------------------------------------------------------------------------------------------------------------
% IMPORTS
%------------------------------------------------------------------------------------------------------------------------

:- discontiguous token/3, csyms/3, csymf/3.
:- table variable_declaration_list/3, statement_list/3, else_if_list/3, 
    simple_expression/3, and_expression/3, sum_expression/3, 
    multiplication_expression/3, mutable/3.

%------------------------------------------------------------------------------------------------------------------------
% COMPILER
%------------------------------------------------------------------------------------------------------------------------

yepl(FileName) :- open(FileName, read, InStream),
		  read_string(InStream, _, InputString),
		  string_to_list(InputString, Codes),
		  lexer(Codes, Tokens),
		  parser(ParseTree, Tokens, []),
		  close(InStream),
		  writeln("Compilation successful!"),
		  split_string(FileName, ".", "", L),
		  L = [H|_T],
		  atom_concat(H, ".ic", X),
		  open(X, write, OutStream),
		  writeq(OutStream, ParseTree),
		  write(OutStream, '.'),
		  close(OutStream).

%------------------------------------------------------------------------------------------------------------------------
% LEXICAL ANALYZER
%------------------------------------------------------------------------------------------------------------------------

% Keywords
token("if") --> "if".
token("elsif") --> "elsif".
token("else") --> "else".
token("print") --> "print".

% Data Types
token("int") --> "int".
token("bool") --> "bool".
token("string") --> "string".

% Constants
token("true") --> "true".
token("false") --> "false".

% Operators
token("<") --> "<".
token(">") --> ">".
token("=") --> "=".
token("++") --> "++".
token("--") --> "--".
token("or") --> "or".
token("||") --> "||".
token("and") --> "and".
token("&&") --> "&&".
token("not") --> "not".
token("!") --> "!".
token("+") --> "+".
token("-") --> "-".
token("*") --> "*".
token("/") --> "/".
token("%") --> "%".
token("+") --> "+".
token("-") --> "-".
token(":") --> ":".
token("?") --> "?".

% Delimiters
token(";") --> ";".
token(",") --> ",".
token(".") --> ".".
token(String, true) -->
    "\"",
    stringContent(Codes),
    "\"",
    {
        string_chars(String, Codes)
    }.
stringContent([C|Chars]) -->
    stringChar(C), stringContent(Chars).
stringContent([]) --> [].

stringChar(0'\n) --> "\\n".
stringChar(0'\t) --> "\\t".
stringChar(0'\") --> "\\\"".
stringChar(0'\") --> "\\\\".
stringChar(C) --> [C].

% Paranthesis
token("[") --> "[".
token("]") --> "]".
token("{") --> "{".
token("}") --> "}".
token("(") --> "(".
token(")") --> ")".

% Loops
token("while") --> "while".
token("for") --> "for".
token("in") --> "in".
token("range") --> "range".

% For processing strings and characters
token(N) --> csymf(C), csyms(Cs), {atom_codes(N,[C|Cs])}.
csymf(C) --> [C], {char_type(C,csymf)}.
csyms([C|Cs]) --> [C], {char_type(C,csym)}, csyms(Cs).
csyms([]) --> [].

% For processing numbers
token(N) --> csymfa(C), csymsa(Cs), {number_codes(N,[C|Cs])}.
csymfa(C) --> [C], {is_digit(C)}.
csymsa([C|Cs]) --> [C], {is_digit(C)}, csymsa(Cs).
csymsa([]) --> [].

% Remove space and newline characters
tokens([]) --> [].
tokens(Ts) --> " ", tokens(Ts).
tokens(Ts) --> "\n", tokens(Ts).
tokens(Ts) --> "\t", tokens(Ts).
tokens(Ts) --> "\r", tokens(Ts).
tokens(Ts) --> "\b", tokens(Ts).
tokens(["\"", T, "\""|Ts]) --> token(T, true), tokens(Ts).
tokens([T|Ts]) --> token(T), tokens(Ts).

lexer(Cs, Tokens) :-
    phrase(tokens(Tokens), Cs), !.

%------------------------------------------------------------------------------------------------------------------------
% PARSER
%------------------------------------------------------------------------------------------------------------------------

% Rule for initiating parser
parser(t_parser(P)) --> program(P).

% Rule for program
program(t_program(B)) --> block(B).

% Rule for Block
block(t_block(DL,SL)) --> declaration_list(DL),[";"],statement_list(SL).
block(t_decl_block(DL)) --> declaration_list(DL),[";"].
block(t_stmt_block(SL)) --> statement_list(SL).
block(t_eps_block("epsilon")) --> [].

% Rules for declaration list
declaration_list(t_declaration_list(D,DL)) --> declaration(D), [";"], declaration_list(DL).
declaration_list(D) --> declaration(D).

% Rule for variable declaration
declaration(t_declaration(TS,VDL)) --> type_specifier(TS), variable_declaration_list(VDL).

% Rules for type specifier
type_specifier(t_type_specifier("int")) --> ["int"].
type_specifier(t_type_specifier("bool")) --> ["bool"].
type_specifier(t_type_specifier("string")) --> ["string"].

% Rules for variable declaration list
variable_declaration_list(t_variable_declaration_list(VDL,VDIN)) --> 
    variable_declaration_list(VDL), [","], variable_declaration_initialize(VDIN).
variable_declaration_list(VDIN) --> 
    variable_declaration_initialize(VDIN).

% Rules for variable declaration initialization
variable_declaration_initialize(t_variable_declaration(VDI)) --> 
    variable_declaration_id(VDI).
variable_declaration_initialize(t_variable_initialize(VDI,SE)) --> 
    variable_declaration_id(VDI), ["="], 								       
    simple_expression(SE).

% Rules for variable declaration id
variable_declaration_id(ID) --> id(ID).

% Rules for statement
statement(ES) --> expression_statement(ES).
statement(CS) --> compound_statement(CS).
statement(SS) --> selection_statement(SS).
statement(IS) --> iteration_statement(IS).
statement(PS) --> print_statement(PS).

%statement(t_statement("epsilon")) --> [].
%statement(B) --> block(B).

% Rules for expression statement
expression_statement(t_expr_stmt(E)) --> expression(E), [";"].
expression_statement(t_empty_expr_stmt) --> [";"].

% Rules for compound statement
compound_statement(t_cmpnd_stmt(SL)) --> ["{"], statement_list(SL), ["}"].
compound_statement(t_empty_cmpnd_stmt) --> ["{"], ["}"].

% Rules for statement list
statement_list(t_stmt_list(SL,S)) --> statement_list(SL), statement(S).
statement_list(S) --> statement(S).

% Rules for else if list
else_if_list(t_else_if_list(ELIFL,ELIF)) --> 
    else_if_list(ELIFL), else_if_list(ELIF). 
else_if_list(t_else_if(SE,S)) --> 
    ["elseif"], ["("],
    simple_expression(SE), [")"],
    statement(S).

% Rules for selection statement
selection_statement(t_sel_stmt(SE,S,ELIFL)) --> 
    ["if"], ["("], simple_expression(SE),[")"], 
    statement(S), 
    else_if_list(ELIFL).
selection_statement(t_sel_stmt(SE,S1,ELIFL,S2)) --> 
    ["if"],["("], simple_expression(SE),[")"], 
    statement(S1),else_if_list(ELIFL), 
    ["else"], statement(S2).

selection_statement(t_sel_stmt(SE,S)) --> 
    ["if"], ["("], simple_expression(SE),[")"], 
    statement(S).
selection_statement(t_sel_stmt(SE,S1,S2)) --> 
    ["if"],["("], simple_expression(SE),[")"], 
    statement(S1), 
    ["else"], statement(S2).

% Rules for iteration range
iteration_range(t_iter_range(ID1,SE1,ID2,RO,SE2)) --> 
    ["("], mutable(ID1), ["="], simple_expression(SE1), [";"], 
    mutable(ID2), relational_operation(RO),
    simple_expression(SE2), [";"], [")"].

iteration_range(t_iter_range(ID2,RO,SE2)) --> 
    ["("], [";"], 
    mutable(ID2), relational_operation(RO),
    simple_expression(SE2), [";"], [")"].

iteration_range(t_iter_range(ID1,SE1,ID2,RO,SE2,E)) --> 
    ["("], mutable(ID1), ["="], simple_expression(SE1), [";"], 
    mutable(ID2), relational_operation(RO), simple_expression(SE2),
    [";"], expression(E), [")"].
iteration_range(t_iter_range(ID,SE1,SE2)) -->  
    mutable(ID), ["in"], ["range"], ["("],
    simple_expression(SE1), [","],
    simple_expression(SE2), [")"].

% Rules for iteration statement
iteration_statement(t_while_stmt(SE,S)) --> 
    ["while"], ["("], simple_expression(SE), [")"], statement(S).
iteration_statement(t_for_stmt(IR,S)) --> 
    ["for"], iteration_range(IR), statement(S).

% Rules for print statement
print_statement(t_print_stmt(SE)) --> ["print"], ["("], simple_expression(SE), [")"], [";"].

% Rules for expression
expression(t_assignment(M,E)) --> mutable(M), ["="], expression(E).
expression(t_addassign(M,E)) --> mutable(M), ["+","="], expression(E).
expression(t_subassign(M,E)) --> mutable(M), ["-","="], expression(E).
expression(t_multassign(M,E)) --> mutable(M), ["*","="], expression(E).
expression(t_divassign(M,E)) --> mutable(M), ["/","="], expression(E).
expression(t_increment(M)) --> mutable(M), ["++"].
expression(t_decrement(M)) --> mutable(M), ["--"].
expression(TE) --> ternary_expression(TE).

% Rules for ternary expression
ternary_expression(t_tern_expr(SE,E1,E2)) -->
    simple_expression(SE), ["?"],
    expression(E1), [":"],
    expression(E2).
ternary_expression(TE) --> simple_expression(TE).

% Rules for simple expression
simple_expression(t_or_expr(SE,AE)) --> simple_expression(SE), ["or"], and_expression(AE).
simple_expression(t_or_expr(SE,AE)) --> simple_expression(SE), ["||"], and_expression(AE).
simple_expression(AE) --> and_expression(AE).

% Rules for and expression
and_expression(t_and_expr(AE,URL)) --> and_expression(AE), ["and"], unary_relational_expression(URL).
and_expression(t_and_expr(AE,URL)) --> and_expression(AE), ["&&"], unary_relational_expression(URL).
and_expression(URL) --> unary_relational_expression(URL).

% Rules for unary expression
unary_relational_expression(t_not_expr(URL)) --> ["not"], unary_relational_expression(URL).
unary_relational_expression(t_not_expr(URL)) --> ["!"], unary_relational_expression(URL).
unary_relational_expression(URL) --> relational_expression(URL).

% Rules for relational expression
relational_expression(t_rel_expr(SE1,RO,SE2)) --> 
    sum_expression(SE1), relational_operation(RO), sum_expression(SE2).
relational_expression(SE) --> sum_expression(SE).

% Rules for relational operation
relational_operation(t_less_than_eq_op("<=")) --> ["<","="].
relational_operation(t_less_than_op("<")) --> ["<"].
relational_operation(t_greater_than_op(">")) --> [">"].
relational_operation(t_greater_than_eq_op(">=")) --> [">","="].
relational_operation(t_eq_op("==")) --> ["=","="].
relational_operation(t_not_eq_op("!=")) --> ["!","="].

% Rules for sum expression
sum_expression(t_sum_expr(SE,SO,ME)) --> 
    sum_expression(SE), sum_operation(SO), multiplication_expression(ME).
sum_expression(ME) --> multiplication_expression(ME).

% Rules for sum operation
sum_operation(t_add_op("+")) --> ["+"].
sum_operation(t_sub_op("-")) --> ["-"].

% Rules for multiplication expression
multiplication_expression(t_mult_expr(ME,MO,UE)) --> 
    multiplication_expression(ME), multiplication_operation(MO), unary_expression(UE).
multiplication_expression(UE) --> unary_expression(UE).

% Rules for multiplication operation
multiplication_operation(t_mult_op("*")) --> ["*"].
multiplication_operation(t_div_op("/")) --> ["/"].
multiplication_operation(t_mod_op("%")) --> ["%"].

% Rules for unary expression
unary_expression(t_unary_expr(UO,UE)) --> unary_operation(UO), unary_expression(UE).
unary_expression(F) --> factor(F).

% Rules for unary operation(-1, +3)
unary_operation(t_unary_op("-")) --> ["-"].
unary_operation(t_unary_op("+")) --> ["+"].

% Rules for factor
factor(IM) --> immutable(IM).
factor(M) --> mutable(M).

% Rules for mutable objects
mutable(t_mutable(ID)) --> id(ID).

% Rules for immutable objects
immutable(t_eval_expr(E)) --> ["("], expression(E), [")"].
immutable(t_const(CONST)) --> constant(CONST).

% Rules for constant
constant(NC) --> num_constant(NC).
constant(SC) --> string_constant(SC).
constant(t_bool_const("true")) --> ["true"].
constant(t_bool_const("false")) --> ["false"].

% Rules for identifier
id(t_id(L)) --> [L], {
    atom_chars(L, Cs),
    length(Cs, N),
    length(Lowers,N),
    maplist(=(lower), Lowers),
    maplist(char_type, Cs, Lowers)}.

% Rules for integer constant
num_constant(t_num_const(L)) --> [L], {
    atom_chars(L, Cs),
    length(Cs, N),
    length(Lowers,N),
    maplist(=(digit), Lowers),
    maplist(char_type, Cs, Lowers)}.

% Rules for string constant
string_constant(t_string_const(SC)) --> ["\""], [SC2], {atom_string(SC, SC2)}, ["\""], !.


