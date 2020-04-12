% @author Aditya Bajaj
% @author Aihaab Shaikh
% @author Sakshi Jain
% @author Sukhpreet Singh Anand
% @version 1.2
% @purpose A lexer to parse the source file and generate tokens and
%          parser to consume the tokens to generate the parse tree.
% @date 04/12/2020

%------------------------------------------------------------------------------------------------------------------------
% LEXICAL ANALYZER
%------------------------------------------------------------------------------------------------------------------------

% TODO

%------------------------------------------------------------------------------------------------------------------------
% PARSER
%------------------------------------------------------------------------------------------------------------------------

:- table declaration_list/3, variable_declaration_list/3, parameter_list/3, statement_list/3,
    local_declarations/3, else_if_list/3, simple_expression/3, and_expression/3, addsub_expresssion/3,
    muldiv_expression/3, mutable/3, argument_list/3.


% Rule for head of parser
parser(t_parser(P)) --> program(P).

% Rule for program
program(t_program(DL)) --> declaration_list(DL).

% Rules for declaration list
declaration_list(t_declaration_list(DL,D)) --> declaration_list(DL), declaration(D).
declaration_list(t_declaration_list(D)) --> declaration(D).

% Rules for declaration
declaration(t_declaration(VD)) --> variable_declaration(VD).
declaration(t_declaration(FD)) --> function_declaration(FD).



% Rule for variable declaration
variable_declaration(t_variable_declaration(TS,VDL)) --> type_specifier(TS), variable_declaration_list(VDL), [";"].

% Rules for variable declaration list
variable_declaration_list(t_variable_declaration_list(VDL,VDIN)) --> variable_declaration_list(VDL), [","], variable_declaration_initialize(VDIN).
variable_declaration_list(t_variable_declaration_list(VDIN)) --> variable_declaration_initialize(VDIN).

% Rules for variable declaration initialization
variable_declaration_initialize(t_variable_declaration_initialize(VDI)) --> variable_declaration_id(VDI).
variable_declaration_initialize(t_variable_declaration_initialize(VDI,SE)) --> variable_declaration_id(VDI), ["="], 
									       simple_expression(SE).
										   
% Rules for variable declaration id
variable_declaration_id(t_variable_declaration_id(ID)) --> id(ID).
variable_declaration_id(t_variable_declaration_id(ID,NC)) --> id(ID), ["["], numconst(NC), ["]"].




% Rules for function declaration
function_declaration(t_function_declaration(TS,ID,PRMS,ST)) --> type_specifier(TS), id(ID), 
							        ["("], parameters(PRMS), [")"], statement(ST).
function_declaration(t_function_declaration(ID,PRMS,ST)) --> id(ID), ["("], parameters(PRMS), [")"], statement(ST).




% Rules for parameters
parameters(t_parameters(PRML)) --> parameter_list(PRML).
parameters(t_parameters("epsilon")) --> [].

% Rules for parameter list
parameter_list(t_parameter_list(PRML,PRM)) --> parameter_list(PRML),[","], parameter(PRM).
parameter_list(t_parameter_list(PRM)) --> parameter(PRM).

% Rules for parameter
parameter(t_parameter(TS,ID)) --> type_specifier(TS), id(ID). 
parameter(t_parameter(TS,ID)) --> type_specifier(TS), id(ID),["["], ["]"]. 




% Rules for statement list
statement_list(t_statement_list(SL,S)) --> statement_list(SL), statement(S).
statement_list(t_statement_list("epsilon")) --> [].

% Rules for statement
statement(t_statement(ES)) --> expression_statement(ES).
statement(t_statement(CS)) --> compound_statement(CS).
statement(t_statement(SS)) --> selection_statement(SS).
statement(t_statement(IS)) --> iteration_statement(IS).
statement(t_statement(RS)) --> return_statement(RS).
statement(t_statement(BS)) --> break_statement(BS).
statement(t_statement(PS)) --> print_statement(PS).




% Rules for compound statement
compound_statement(t_compound_statement(LD,SL)) --> ["{"], local_declarations(LD), statement_list(SL), ["}"].

% Rules for local declarations
local_declarations(t_local_declaration(LD1,LD2)) --> local_declarations(LD1), local_declaration(LD2).
local_declarations(t_local_declaration("epsilon")) --> [].

% Rules for local declaration
local_declaration(t_local_declaration(TS,ID)) --> type_specifier(TS), id(ID), [";"].
local_declaration(t_local_declaration(TS,ID)) --> type_specifier(TS), id(ID), ["["], ["]"], [";"].




% Rules for selection statement
selection_statement(t_selection_statement(SE,S,ELIFL)) --> ["if"],["("], simple_expression(SE), [")"],
															statement(S),else_if_list(ELIFL).
selection_statement(t_selection_statement(SE,S1,ELIFL,S2)) --> ["if"],["("], simple_expression(SE), [")"], 
																statement(S1), else_if_list(ELIFL), ["else"], statement(S2).

% Rules for else if list
else_if_list(t_else_if_list(ELIFL,SE,S)) --> else_if_list(ELIFL), ["elsif"],["("], simple_expression(SE), [")"], statement(S).
else_if_list(t_else_if_list("epsilon")) --> [].




% Rules for iteration statement
iteration_statement(t_iteration_statement(SE,S)) --> ["while"], ["("], simple_expression(SE), [")"], statement(S).
iteration_statement(t_iteration_statement(IR,S)) --> ["for"], iteration_range(IR), statement(S).

% Rules for iteration range
iteration_range(t_iteration_range(ID,SE1,RO,SE2)) --> ["("], id(ID), ["="], simple_expression(SE1), [";"], id(ID),
													relational_operation(RO), simple_expression(SE2),[";"],[")"].

iteration_range(t_iteration_range(ID,SE1,RO,SE2,E)) --> ["("], id(ID), ["="], simple_expression(SE1), [";"], 
													id(ID),relational_operation(RO), simple_expression(SE2),[";"],
													expression(E),[")"].

iteration_range(t_iteration_range(ID,SE1,SE2)) -->  id(ID),["in"],["range"],["("], simple_expression(SE1), [","], 
													simple_expression(SE2),[")"].				 
							 


% Rules for return statement
return_statement(t_return_statement("return")) --> ["return"], [";"].
return_statement(t_return_statement(E)) --> ["return"], expression(E), [";"].

% Rules for break statement
break_statement("break") --> ["break"], [";"].

% Rules for print statement
print_statement(t_print_statement()) --> ["print"],["("] simple_expression(SE), [")"],[";"].




% Rules for expression statement
expression_statement(t_expression_statement(E)) --> expression(E), [";"].
expression_statement(t_expression_statement(;)) --> [";"].


% Rules for expression
expression(t_expression(M,E)) --> mutable(M), ["="], expression(E).
expression(t_expression(M,E)) --> mutable(M), ["+","="], expression(E).
expression(t_expression(M,E)) --> mutable(M), ["-","="], expression(E).
expression(t_expression(M,E)) --> mutable(M), ["*","="], expression(E).
expression(t_expression(M,E)) --> mutable(M), ["/","="], expression(E).
expression(t_expression(M)) --> mutable(M), ["+","+"].
expression(t_expression(M)) --> mutable(M), ["-","-"].
expression(t_expression(SE)) --> simple_expression(SE).

% Rules for simple expression
simple_expression(t_simple_expression(SE,OR,AE)) --> simple_expression(SE), or_op(OR), and_expression(AE).
simple_expression(t_simple_expression(AE)) --> and_expression(AE).

% Rules for and expression
and_expression(t_and_expression(AE,AND,URL)) --> and_expression(AE), and_op(AND), unary_relational_expression(URL).
and_expression(t_and_expression(URL)) --> unary_relational_expression(URL).

% Rules for unary expression
unary_relational_expression(t_unary_relational_expression(NOT,URL)) --> not_op(NOT), unary_relational_expression(URL).
unary_relational_expression(t_unary_relational_expression(RL)) --> relational_expression(RL).



% Rules for relational expression
relational_expression(t_relational_expression(SE1,RO,SE2)) --> addsub_expresssion(SE1), 
							       relational_operation(RO), 
							       addsub_expresssion(SE2).
relational_expression(t_relational_expression(SE)) --> addsub_expresssion(SE).

% Rules for addsub expresssion 
addsub_expresssion(t_sum_expression(SE,SO,ME)) --> addsub_expresssion(SE), 
					       addsub_operation(SO), 
					       muldiv_expression(ME).
addsub_expresssion(t_sum_expression(ME)) --> muldiv_expression(ME).

% Rules for sum operation
addsub_operation(t_addsub_operation("+")) --> ["+"].
addsub_operation(t_addsub_operation("-")) --> ["-"].


% Rules for multiplication expression
muldiv_expression(t_multiplication_expression(ME,MO,UE)) --> muldiv_expression(ME), 
								     muldiv_operation(MO), 
								     unary_expression(UE).
muldiv_expression(t_multiplication_expression(UE)) --> unary_expression(UE).

% Rules for multiplication operation
muldiv_operation(t_muldiv_operation("*")) --> ["*"].
muldiv_operation(t_muldiv_operation("/")) --> ["/"].
muldiv_operation(t_muldiv_operation("%")) --> ["%"].




% Rules for unary expression
unary_expression(t_unary_expression(UO,UE)) --> unary_operation(UO), unary_expression(UE).
unary_expression(t_unary_expression(F)) --> factor(F).

% Rules for unary operation
unary_operation(t_unary_operation("-")) --> ["-"].
unary_operation(t_unary_operation("*")) --> ["*"].
%unary_operation(t_unary_operation("?")) --> ["?"].

% Rules for factor
factor(t_factor(IM)) --> immutable(IM).
factor(t_factor(M)) --> mutable(M).

% Rules for mutable objects
mutable(t_mutable(ID)) --> id(ID).
mutable(t_mutable(M,E)) --> mutable(M), ["["], expression(E), ["]"].

% Rules for immutable objects
immutable(t_immutable(E)) --> ["("], expression(E), [")"].
immutable(t_immutable(ID)) --> id(ID),["."],["size"].
immutable(t_immutable(C)) --> function_call(C).
immutable(t_immutable(CONST)) --> constant(CONST).


% Rules for function call
function_call(t_call(ID,ARG)) --> id(ID), ["("], arguments(ARG), [")"].

% Rules for arguments
arguments(t_arguments(ARGL)) --> argument_list(ARGL).
arguments(t_arguments("epsilon")) --> [].

% Rules for argument list
argument_list(t_argument_list(ARGL,E)) --> argument_list(ARGL), [","], expression(E).
argument_list(t_argument_list(E)) --> expression(E).


% Rules for constant
constant(t_constant(NC)) --> num_constant(NC).
constant(t_constant(CC)) --> char_constant(CC).
constant(t_constant(SC)) --> string_constant(SC).
constant(t_constant("true")) --> ["true"].
constant(t_constant("false")) --> ["false"].



% Rules for relational operation
relational_operation(t_relational_operation("<=")) --> ["<","="].
relational_operation(t_relational_operation("<")) --> ["<"].
relational_operation(t_relational_operation(">")) --> [">"].
relational_operation(t_relational_operation(">=")) --> [">","="].
relational_operation(t_relational_operation("==")) --> ["=","="].
relational_operation(t_relational_operation("!=")) --> ["!","="].

% Rules for OR operation
or_op(t_or("or")) --> ["or"].
or_op(t_or("||")) --> ["||"].

% Rules for AND operation
and_op(t_and("and")) --> ["and"].
and_op(t_and("&&")) --> ["&&"].

% Rules for NOT operation
not_op(t_not("not")) --> ["not"].
not_op(t_not("!")) --> ["!"].


% Rules for type specifier
type_specifier(t_ts("int")) --> ["int"].
type_specifier(t_ts("bool")) --> ["bool"].
type_specifier(t_ts("char")) --> ["char"]
type_specifier(t_ts("string")) --> ["string"].


% Rules for identifier
id(t_id(I)) --> [I], {re_match("^[a-zA-Z_$][a-zA-Z_$0-9]*$", I)}.

% Rules for integer constant
num_constant(t_num_constant(NC)) --> [NC], {rangeValidatorInt(NC)}.

% Rules for character constant
char_constant(t_char_constant(CC)) --> [CC], {re_match("'[\x00-\x7F]'", CC)}.

% Rules for string constant
string_constant(t_string_constant(SC)) --> [SC], {re_match("\"[\x00-\x7F]*\"", SC)}.

% Predicate for validating integer range
rangeValidatorInt(NC) :- NC >= -2147483648, NC =< 2147483647.
