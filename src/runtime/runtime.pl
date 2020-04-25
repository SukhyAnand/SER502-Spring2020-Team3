% @author Sukhpreet Singh Anand
% @author Sakshi Jain
% @author Aditya Bajaj
% @author Aihaab Shaikh
% @version 1.0
% @purpose A runtime environment which parses the intermediate abstract syntax tree code and executes the program
% @date 04/01/2020

/*
lookup(Id, [], _Type,_Val):-
    write(Id), write(" doesnt exist"),
    writeln("Ref to unitialized variable!"),
    fail.
*/
lookup(Id, [(Id,Type,Val)|_],Type,Val).
lookup(Id, [_|T],Type, Val):- lookup(Id,T,Type,Val). 

update(Id, Type, NewVal, [], [(Id, Type,NewVal)]). %if not found
update(Id, Type,NewVal, [(Id,Type,_Val)|T], [(Id,Type,NewVal)|T]). %if found

update(Id, Type, NewVal, [H|T], [H|R]):-
    H\=(Id,,),update(Id, Type, NewVal,T,R).



interpreter(P) :- eval_program(P).

eval_program(t_program(B)) :- eval_block(B,[],_).

%Block Evaluations
eval_block(t_block(DL,S),Env, EnvF) :- eval_dec_list(DL,Env,Env1), eval_stmt_list(S,Env1,EnvF).
eval_block(t_decl_block(DL),Env, EnvF) :- eval_dec_list(DL,Env,EnvF).
eval_block(t_stmt_block(S),Env, EnvF) :- eval_stmt_list(S,Env,EnvF).
eval_block(t_eps_block("epsilon"),Env, Env).	


%Evaluator - Declarations 

eval_dec_list(t_declaration_list(D,DL),Env, EnvF) :- eval_dec(D,Env,Env1),eval_dec_list(DL,Env1,EnvF).
eval_dec_list(D,Env, EnvF) :- eval_dec(D,Env,EnvF).
eval_dec_list(t_declaration_list("epsilon"),Env, Env).	

eval_dec(t_declaration(TS,VDL), Env,EnvF) :- 
    eval_type_specifier(TS, Type), 
    eval_var_dec_list(VDL,Type, Env,EnvF).

eval_type_specifier(t_type_specifier(TS),TS).


%Variable Declarations 

eval_var_dec_list(t_variable_declaration_list(VDL,VDIN),Type,Env,EnvF):- 
    eval_var_dec_list(VDL,Type,Env,Env1),
    eval_var_dec_initialize(VDIN,Type,Env1,EnvF).

eval_var_dec_list(VDIN,Type,Env,EnvF):-eval_var_dec_initialize(VDIN,Type,Env,EnvF).


%if not found in env
eval_var_dec_initialize(t_variable_declaration(Id),Type,Env,EnvF):- 
    eval_var_dec_id(Id,Idname),
    \+lookup(Idname,Env,,),
    type_default(Type,Def_Val),
    update(Idname,Type,Def_Val,Env,EnvF).

%if not found in env ie re-declaring a variable
eval_var_dec_initialize(t_variable_declaration(Id),_,Env,Env):- 
    eval_var_dec_id(Id,Idname),
    lookup(Idname,Env,,),
    write(Id), write(" Already exist"),
    writeln("Give new variable name!"),
    fail.

%if not found in env
eval_var_dec_initialize(t_variable_initialize(Id,SE),Type,Env,EnvF):- 
    eval_var_dec_id(Id,Idname),
    \+lookup(Idname,Env,,),
    eval_simple_expr(SE,Env,Env1,Val),
    update(Idname,Type,Val,Env1,EnvF).

%if not found in env ie re-declaring a variable
eval_var_dec_initialize(t_variable_initialize(Id,),,Env,Env):- 
    eval_var_dec_id(Id,Idname),
    lookup(Idname,Env,,),
    write(Id), write(" Already exist"),
    writeln("Give new variable name!"),
    fail.

eval_var_dec_id(I,Idname):- eval_id_name(I,Idname).


%Evaluator - Statements

eval_stmt(ES,Env,EnvF) :- eval_expr_stmt(ES,Env,EnvF).
eval_stmt(CS,Env,EnvF) :- eval_cmpnd_stmt(CS,Env,EnvF).
eval_stmt(SS,Env,EnvF) :- eval_sel_stmt(SS,Env,EnvF).
eval_stmt(t_iter_stmt(IS),Env,EnvF) :- eval_iter_stmt(IS,Env,EnvF).
eval_stmt(t_print_stmt(PS),Env,EnvF) :- eval_print_stmt(PS,Env,EnvF).

eval_print_stmt(PS,Env,EnvF):- 
    eval_simple_expr(PS,Env,EnvF,V),
    write(V).
    

% Expression Statement Evaluator

eval_expr_stmt(t_expr_stmt(E),Env,EnvF) :- eval_expr(E,Env,EnvF,_Val).
eval_expr_stmt(t_expression_statement(;),Env,Env).


% Compound Statement Evaluator

eval_cmpnd_stmt(t_cmpnd_stmt(E),Env,EnvF) :- eval_stmt_list(E,Env,EnvF).

eval_stmt_list(t_stmt_list(S,SL),Env,EnvF) :- eval_stmt(S,Env,Env1),eval_stmt_list(SL,Env1,EnvF).
eval_stmt_list(t_stmt_list("epsilon"),Env, Env).	
eval_stmt_list(S,Env,EnvF) :- eval_stmt(S,Env,EnvF).




% Selection Statement Evaluator

eval_sel_stmt(t_sel_stmt(SE,S,_E),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S,Env1,EnvF).

eval_sel_stmt(t_sel_stmt(SE,_S,E),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,false),
    eval_elseif(E,Env1,EnvF,_Val).

eval_sel_stmt(t_sel_stmt(SE,S1,_E,_S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S1,Env1,EnvF).
 %   eval_elseif(E,Env1,EnvF).

eval_sel_stmt(t_sel_stmt(SE,_S1,E,S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val1),
    boolval(Val1,false),
    eval_elseif(E,Env1,Env2,Val2),
    boolval(Val2,false),
    eval_stmt(S2,Env2,EnvF).


% Elseif Evaluator

eval_elseif(t_else_if_list(E,SE,S),Env,EnvF,Val) :- 
    eval_elseif(E,Env,Env1,Val),
    boolval(Val,false),
    eval_simple_expr(SE,Env1,Env2,Val2),
    boolval(Val2,true),
    eval_stmt(S,Env2,EnvF).

eval_elseif(t_else_if_list(E,_SE,_S),Env,EnvF,true) :- 
    eval_elseif(E,Env,EnvF,Val),
    boolval(Val,true).


eval_elseif(t_else_if_list("epsilon"),Env, Env,false).	

/*
 * % Rules for iteration statement
iteration_statement(t_iteration_statement(SE,S)) --> 
    ["while"],["("], simple_expression(SE), [")"], statement(S).
iteration_statement(t_iteration_statement(IR,S)) --> 
    ["for"], iteration_range(IR), statement(S).
 * 
 * % Rules for iteration range
iteration_range(t_iteration_range(ID1,SE1,ID2,RO,SE2)) --> 
    ["("], id(ID1), ["="], simple_expression(SE1), [";"], 
    id(ID2), relational_operation(RO),
    simple_expression(SE2),[";"], [")"].
iteration_range(t_iteration_range(ID1,SE1,ID2,RO,SE2,E)) --> 
    ["("], id(ID1), ["="], simple_expression(SE1), [";"], 
    id(ID2), relational_operation(RO), simple_expression(SE2),
    [";"],expression(E),[")"].
iteration_range(t_iteration_range(ID,SE1,SE2)) -->  
    id(ID), ["in"],["range"],["("],
    simple_expression(SE1),[","],
    simple_expression(SE2),[")"].
 * */

eval_iter_range(t_iteration_range(SE), Env, EnvF, Val) :-
    eval_simple_expr(SE,Env,EnvF,Val).

eval_iter_stmt(t_while_statement(SE,_S), Env, EnvF) :- 
    eval_simple_expr(SE,Env,EnvF,Val), boolval(Val, false).

eval_iter_stmt(t_while_statement(SE,S), Env, EnvF) :- 
    eval_simple_expr(SE,Env,Env1,Val), boolval(Val, true), 
    eval_stmt(S,Env1,Env2), 
    eval_iter_stmt(t_while_statement(SE,S), Env2, EnvF).

%eval_iter_stmt(t_for_statement(IR,_S), Env, EnvF) :- 
%    eval_iter_range(IR,Env,EnvF,Val), boolval(Val, false).

%eval_iter_stmt(t_for_statement(IR,S), Env, EnvF, Flag) :- 
%    eval_iter_range(IR,Env,Env1,Val,Flag), boolval(Val,true), 
%    eval_stmt(S,Env1,Env2), 
%    eval_iter_stmt(t_for_statement(IR,S), Env2, EnvF, False).

% for (; i < 10;)
eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), _S), Env, EnvF) :-
    eval_id_name(ID2, Idname), lookup(Idname, Env, "int", _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, EnvF, Val), boolval(Val, false).

% for (; i < 10;)
eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), S), Env, EnvF) :-
    eval_id_name(ID2, Idname), lookup(Idname, Env, "int", _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, Env1, Val), boolval(Val, true),
    eval_stmt(S, Env1, Env2),
    eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), S), Env2, EnvF).

%for (i = 0; i < 10;)
eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,ID2,RO,SE2),_S), Env, EnvF) :-
    eval_simple_expr(SE1, Env, Env1, Val1), eval_id_name(ID1, Idname), 
    lookup(Idname, Env1, "int", _),
    update(Idname, "int", Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, EnvF, Val2), boolval(Val2, false).

%for (i = 0; i < 10;)
eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,ID2,RO,SE2),S), Env, EnvF) :-
	eval_simple_expr(SE1, Env, Env1, Val1), eval_id_name(ID1, Idname), 
    lookup(Idname, Env1, "int", _),
    update(Idname, "int", Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, Env3, Val2), boolval(Val2, true),
    eval_stmt(S, Env3, Env4),
    eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), S), Env4, EnvF).

% for (i < 10; i++)
eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2,_E), _S), Env, EnvF) :-
    eval_id_name(ID2, Idname), lookup(Idname, Env, "int", _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, EnvF, Val), boolval(Val, false).

% for (i < 10; i++)
eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2,E), S), Env, EnvF) :-
    eval_id_name(ID2, Idname), lookup(Idname, Env, "int", _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, Env1, Val), boolval(Val, true),
    eval_stmt(S, Env1, Env2),
    eval_expr(t_expression(E), Env2, Env3, _Val),
    eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), S), Env3, EnvF).

% for (i = 0; i < 10; i++)
eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,ID2,RO,SE2,_E),_S), Env, EnvF) :-
    eval_simple_expr(SE1, Env, Env1, Val1), eval_id_name(ID1, Idname), 
    lookup(Idname, Env1, "int", _),
    update(Idname, "int", Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, EnvF, Val2), boolval(Val2, false).

% for (i = 0; i < 10; i++)
eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,ID2,RO,SE2,E),S), Env, EnvF) :-
	eval_simple_expr(SE1, Env, Env1, Val1), eval_id_name(ID1, Idname), 
    lookup(Idname, Env1, "int", _),
    update(Idname, "int", Val1, Env1, Env2),
%review again!!!!!!!!!!!!
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, Env3, Val2), boolval(Val2, true),
    eval_stmt(S, Env3, Env4),
%review again!!!!!!!!!!!
    eval_expr(t_expression(E), Env4, Env5, _Val),
    eval_iter_stmt(t_for_statement(t_iteration_range(ID2,RO,SE2), S), Env5, EnvF).

% for in range (1, 10)
eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,SE2),S), Env, EnvF) :-
	eval_id_name(ID1, Idname), 
    lookup(Idname, Env, "int", Val),
    eval_simple_expr(SE1, Env, Env1, Val1),
    eval_simple_expr(SE1, Env1, Env2, Val2),
    Val1 < Val2, 
    Val =< Val1, 
    Val1 < Val2, 
    eval_stmt(S, Env2, Env3),
    eval_expr(t_increment(t_mutable(ID1)), Env3, Env4, _Val),
    eval_iter_stmt(t_for_statement(t_iteration_range(ID1,SE1,SE2), S), Env4, EnvF).









% Expression Evaluations

%Assignment expression
eval_expr(t_assignment(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, Type , _),
    update(Idname, Type , Val, Env1, EnvF).

% += Expression 
eval_expr(t_addassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, "int" , Val1),
    %number(Val),
    Val2 is Val1 + Val,
    update(Idname, "int" , Val2, Env1, EnvF).
/*
eval_expr(t_addassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, "string" , Val1),
    Val2 is Val1 + Val,
    update(Idname, "string" , Val2, Env1, EnvF).
**/

% -= Expression
eval_expr(t_subassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, "int" , Val1),
    Val2 is Val1 - Val,
    update(Idname, "int" , Val2, Env1, EnvF). 
% *= Expression
eval_expr(t_multassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, "int"  , Val1),
    Val2 is Val1 * Val,
    update(Idname, "int"  , Val2, Env1, EnvF). 
% /= Expression
eval_expr(t_divassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, "int"  , Val1),
    Val2 is Val1 / Val,
    update(Idname, "int"  , Val2, Env1, EnvF). 
% increment expr
eval_expr(t_increment(M), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname),
    lookup(Idname, Env, "int" , Val),
    Val1 is Val + 1,
    update(Idname, "int" , Val1, Env, EnvF).

% decrement expr
eval_expr(t_decrement(M), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname),
    lookup(Idname, Env, "int" , Val),
    Val1 is Val - 1,
    update(Idname, "int" , Val1, Env, EnvF).

eval_expr(SE, Env, EnvF, Val) :- eval_simple_expr(SE, Env, EnvF, Val).

%or expression
eval_simple_expr(t_or_expr(SE,AE), Env, Env, Val) :-
    eval_simple_expr(SE,Env, Env, Val1), 
    boolval(Val1, Val2),
    eval_and_expr(AE,Env, Env, Val3), 
    Val is Val2 + Val3.
%or expression
eval_simple_expr(AE , Env, Env, Val) :-
    eval_and_expr(AE, Env, Env, Val).

%and expression
eval_and_expr(t_and_expr(SE,AE), Env, Env, Val) :-
    eval_and_expr(SE,Env, Env, Val1),
    eval_unary_rel_expr(AE,Env, Env, Val2),
    boolval(Val2, Val3),
    Val is Val1 , Val3.
%and expression
eval_and_expr(AE , Env, Env, Val) :-
    eval_unary_rel_expr(AE, Env, Env, Val).

%unary rel expressions
eval_unary_rel_expr(t_not_expr(URL),Env, EnvF, Val) :-
    eval_unary_rel_expr(URL, Env, EnvF, Val1),
    boolval(Val1,Val2),
    not(Val2, Val). 

eval_unary_rel_expr(RL ,Env, EnvF, Val) :-
    eval_rel_expr(RL, Env, EnvF, Val).
    

% Relational Expression Evaluations

eval_rel_expr(t_relational_expr(SE1,RO,SE2),Env,EnvF,Val):-
    eval_sum_expr(SE1,Env,Env1,Val1),
    eval_sum_expr(SE2,Env1,EnvF,Val2),
    eval_rel_op(RO,Val1,Val2,Val).
eval_rel_expr(SE,Env,EnvF,Val):-
     eval_sum_expr(SE,Env,EnvF,Val).

% Relational Operator Evaluations
/*
eval_rel_op(t_less_than_eq_op("<="),V1,V2,V):-
    (V1=<V2 -> V is true; V is false).
eval_rel_op(t_less_than_op("<"),V1,V2,V):-
    (V1<V2 -> V is true; V is false).    
eval_rel_op(t_greater_than_eq_op(">="),V1,V2,V):-
    (V1>=V2 -> V is true; V is false).
eval_rel_op(t_greater_than_op(">"),V1,V2,V):-
    (V1>V2 -> V is true; V is false).  
eval_rel_op(t_eq_op("=="),V1,V2,V):-
    (V1==V2 -> V is true; V is false).
eval_rel_op(t_not_eq_op("!="),V1,V2,V):-
    (V1==V2 -> V is false; V is true). 
*/


eval_rel_op(t_less_than_eq_op("<="),V1,V2,true):- V1=<V2.
eval_rel_op(t_less_than_eq_op("<="),V1,V2,false):- V1>V2.

eval_rel_op(t_less_than_op("<"),V1,V2,true):- V1<V2.
eval_rel_op(t_less_than_op("<"),V1,V2,false):- V1>=V2.


eval_rel_op(t_greater_than_eq_op(">="),V1,V2,true):- V1>=V2.
eval_rel_op(t_greater_than_eq_op(">="),V1,V2,false):- V1<V2.

eval_rel_op(t_greater_than_op(">"),V1,V2,true):- V1>V2.
eval_rel_op(t_greater_than_op(">"),V1,V2,false):- V1=<V2.


eval_rel_op(t_eq_op("=="),V1,V2,true):- V1==V2.
eval_rel_op(t_eq_op("=="),V1,V2,false):- V1=\=V2.

eval_rel_op(t_not_eq_op("!="),V1,V2,true):- V1=\=V2.
eval_rel_op(t_not_eq_op("!="),V1,V2,false):- V1==V2.

% Sum Expression Evaluations

eval_sum_expr(t_sum_expr(SE,SO,ME),Env,EnvF,Val):-
    eval_sum_expr(SE,Env,Env1,Val1),
    eval_mult_expr(ME,Env1,EnvF,Val2),
    eval_sum_op(SO,Val1,Val2,Val).
eval_sum_expr(ME,Env,EnvF,Val):-
     eval_mult_expr(ME,Env,EnvF,Val).

% Sum Operator Evaluations

eval_sum_op(t_add_op("+"),Val1,Val2,Val):-
    Val is Val1+Val2.
eval_sum_op(t_sub_op("-"),Val1,Val2,Val):-
    Val is Val1-Val2.


% Mult Expression Evaluations

eval_mult_expr(t_mult_expr(ME,MO,UE),Env,EnvF,Val):-
    eval_mult_expr(ME,Env,Env1,Val1),
    eval_unary_expr(UE,Env1,EnvF,Val2),
    eval_mult_op(MO,Val1,Val2,Val).
eval_mult_expr(UE,Env,EnvF,Val):-
     eval_unary_expr(UE,Env,EnvF,Val).

% Mult Operator Evaluations

eval_mult_op(t_mult_op("*"),Val1,Val2,Val):-
    Val is Val1*Val2.
eval_mult_op(t_div_op("/"),Val1,Val2,Val):-
    Val is Val1/Val2.
eval_mult_op(t_mod_op("%"),Val1,Val2,Val):-
    Val is mod(Val1,Val2).


eval_unary_expr(t_unary_expr(UO,UE),Env,EnvF,Val):-
    eval_unary_expr(UE,Env,EnvF,Val1),
    eval_unary_op(UO,Val1,Val).
eval_unary_expr(F,Env,EnvF,Val):-eval_factor(F,Env,EnvF,Val).

eval_unary_op(t_unary_op("-"),Val1,Val2):-Val2 is (-1)* Val1.
eval_unary_op(t_unary_op("+"),Val1,Val1).



eval_factor(IM, Env,EnvF,Val):- eval_immutable(IM, Env,EnvF,Val).

eval_factor(M,Env,Env, Val):- eval_mutable(M,Env, Env, Val).


eval_mutable_id(t_mutable(M), Val) :- eval_id_name(M,Val).

eval_mutable(t_mutable(M), Env, Env, Val) :- eval_id(M,Env,Env,Val).

eval_id_name(t_id(I),I).
eval_id(t_id(I),Env,Env,Val):- lookup(I,Env,_,Val).

eval_immutable(t_eval_expr(E), Env,EnvF,Val):-eval_expr(E, Env,EnvF,Val).
eval_immutable(t_const(X),,,Val):- eval_const(X,Val).



eval_const(NC,Val):- eval_num_const(NC,Val).
eval_const(t_bool_const("true"),true).
eval_const(t_bool_const("false"),false).


eval_num_const(t_num_const(X),X).





% Not Evaluations

not(true, false).
not(false, true).

% BoolVal Evaluations

boolval(0,false).
boolval('',false).
boolval("",false).
boolval(false,false).
boolval(Val,true) :- Val \= 0, Val \= '', Val \= "", Val \= false. 



% Default Data Type Value Evaluations

type_default("int",0).
type_default("char",'').
type_default("string","").
type_default("bool",false).
