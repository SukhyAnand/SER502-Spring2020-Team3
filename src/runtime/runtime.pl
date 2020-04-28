% @author Sukhpreet Singh Anand
% @author Sakshi Jain
% @author Aditya Bajaj
% @author Aihaab Shaikh
% @version 1.4
% @purpose A runtime environment which parses the intermediate abstract syntax tree code and executes the program
% @date 04/27/2020

% Lookup variable Identifier name, data type and value from the environment
lookup(Id, [(Id,Type,Val)|_],Type,Val).
lookup(Id, [_|T],Type, Val):- lookup(Id,T,Type,Val). 

% Update variable Identifier name, data type and value to the environment
update(Id, Type, NewVal, [], [(Id, Type,NewVal)]). %if not found
update(Id, Type,NewVal, [(Id,Type,_Val)|T], [(Id,Type,NewVal)|T]). %if found
update(Id, Type, NewVal, [H|T], [H|R]):-
    H\=(Id,_,_),update(Id, Type, NewVal,T,R).

% Run the interpreter from the intermediate code file
runYepl(FileName) :- 
    open(FileName, read, InStream), 
    read(InStream, P), 
    close(InStream), interpreter(P).

% Head rule for interpreter
interpreter(P) :- eval_program(P).

% Evaluator for program
eval_program(t_parser(t_program(B))) :- eval_block(B,[],_).

% Evaluator for block
eval_block(t_block(DL,S),Env, EnvF) :- eval_dec_list(DL,Env,Env1), eval_stmt_list(S,Env1,EnvF).
eval_block(t_decl_block(DL),Env, EnvF) :- eval_dec_list(DL,Env,EnvF).
eval_block(t_stmt_block(S),Env, EnvF) :- eval_stmt_list(S,Env,EnvF).
eval_block(t_eps_block("epsilon"),Env, Env).	

% Evaluator for declaration list 
eval_dec_list(t_declaration_list(D,DL),Env, EnvF) :- eval_dec(D,Env,Env1),eval_dec_list(DL,Env1,EnvF).
eval_dec_list(D,Env, EnvF) :- eval_dec(D,Env,EnvF).
eval_dec_list(t_declaration_list("epsilon"),Env, Env).	

% Evaluator for declaration
eval_dec(t_declaration(TS,VDL), Env,EnvF) :- 
    eval_type_specifier(TS, Type), 
    eval_var_dec_list(VDL,Type, Env,EnvF).

% Evaluator for type specifier
eval_type_specifier(t_type_specifier(TS),TS).

% Evaluator for variable declaration list
eval_var_dec_list(t_variable_declaration_list(VDL,VDIN),Type,Env,EnvF):- 
    eval_var_dec_list(VDL,Type,Env,Env1),
    eval_var_dec_initialize(VDIN,Type,Env1,EnvF).
eval_var_dec_list(VDIN,Type,Env,EnvF):-eval_var_dec_initialize(VDIN,Type,Env,EnvF).

% Evaluators for variable declaration and initialization
% If variable not found in environment during declaration
eval_var_dec_initialize(t_variable_declaration(Id),Type,Env,EnvF):- 
    eval_var_dec_id(Id,Idname),
    \+lookup(Idname,Env,_,_),
    type_default(Type,Def_Val),
    update(Idname,Type,Def_Val,Env,EnvF).

% If variable not found in environment, i.e., re-declaring a variable
eval_var_dec_initialize(t_variable_declaration(Id),_,Env,Env):- 
    eval_var_dec_id(Id,Idname),
    lookup(Idname,Env,_,_),
    write(Id), write(" Already exist"),
    writeln("Give new variable name!"),
    fail.

% If variable not found in environment during initialiation
eval_var_dec_initialize(t_variable_initialize(Id,SE),Type,Env,EnvF):- 
    eval_var_dec_id(Id,Idname),
    \+lookup(Idname,Env,_,_),
    eval_simple_expr(SE,Env,Env1,Val),
    %type_check(Val,Type),
    update(Idname,Type,Val,Env1,EnvF).

% If variable not found in environment, i.e., re-declaring a variable for initialization
eval_var_dec_initialize(t_variable_initialize(Id,_),_,Env,Env):- 
    eval_var_dec_id(Id,Idname),
    lookup(Idname,Env,_,_),
    write(Id), write(" Already exist"),
    writeln("Give new variable name!"),
    fail.

% Evaluator for variable declaration id
eval_var_dec_id(I,Idname):- eval_id_name(I,Idname).

% Evaluator for statements
eval_stmt(ES,Env,EnvF) :- eval_expr_stmt(ES,Env,EnvF).
eval_stmt(CS,Env,EnvF) :- eval_cmpnd_stmt(CS,Env,EnvF).
eval_stmt(SS,Env,EnvF) :- eval_sel_stmt(SS,Env,EnvF).
eval_stmt(IS,Env,EnvF) :- eval_iter_stmt(IS,Env,EnvF).
eval_stmt(PS,Env,EnvF) :- eval_print_stmt(PS,Env,EnvF).

% Evaluator for print statement
eval_print_stmt(t_print_stmt(PS),Env,EnvF):- 
    eval_simple_expr(PS,Env,EnvF,V),
    writeln(V).
    
% Evaluators for expression statement
eval_expr_stmt(t_expr_stmt(E),Env,EnvF) :- eval_expr(E,Env,EnvF,_Val).
eval_expr_stmt(t_expr_stmt(;),Env,Env).

% Evaluator for compound statement
eval_cmpnd_stmt(t_cmpnd_stmt(E),Env,EnvF) :- eval_stmt_list(E,Env,EnvF).

% Evaluators for statement list
eval_stmt_list(t_stmt_list(S,SL),Env,EnvF) :- eval_stmt(S,Env,Env1),eval_stmt_list(SL,Env1,EnvF).
eval_stmt_list(t_stmt_list(epsilon),Env, Env).	
eval_stmt_list(S,Env,EnvF) :- eval_stmt(S,Env,EnvF).

% Evaluator for selection statement
%if (true), run stmt
eval_sel_stmt(t_sel_stmt(SE,S),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S,Env1,EnvF).

%if (false), don't run stmt
eval_sel_stmt(t_sel_stmt(SE,_S),Env,EnvF) :-
    eval_simple_expr(SE,Env,EnvF,Val),
    boolval(Val,false).

%if (false) ; elseif exists but no else
eval_sel_stmt(t_sel_stmt(SE,_S,E),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,false),
    eval_elseif(E,Env1,EnvF,_Val).
   
%if (true), else    
eval_sel_stmt(t_sel_stmt(SE,S1,_E,_S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S1,Env1,EnvF).

%if (false), elseif(false), else    
eval_sel_stmt(t_sel_stmt(SE,_S1,E,S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val1),
    boolval(Val1,false),
    eval_elseif(E,Env1,Env2,Val2),
    boolval(Val2,false),
    eval_stmt(S2,Env2,EnvF).
    
%if (true), run stmt  ; else stmt ; no elseif stmt is present
eval_sel_stmt(t_sel_stmt(SE,S1,_S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S1,Env1,EnvF).

%if (false), don't run stmt ; else stmt ; no elseif stmt is present
eval_sel_stmt(t_sel_stmt(SE,_S1,S2),Env,EnvF) :-
    eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,false),
    eval_stmt(S2,Env1,EnvF).

% Evaluator for else if block
% multiple elseif lists
eval_elseif(t_else_if_list(EL,E),Env,EnvF,Val) :- 
    eval_elseif(t_else_if_list(EL),Env,Env1,Val1),
    boolval(Val1,false),
    eval_elseif(E,Env1,EnvF,Val).
eval_elseif(t_else_if_list(EL,_E),Env,EnvF,Val) :- 
    eval_elseif(t_else_if_list(EL),Env,EnvF,Val),
    boolval(Val,true).

% rules for processing elseif list when it has only 1 elseif block
eval_elseif(t_else_if_list(EL,_E),Env,EnvF,Val) :- 
    eval_elseif(EL,Env,EnvF,Val),
    boolval(Val,true),!.
eval_elseif(t_else_if_list(EL,E),Env,EnvF,Val) :- 
    eval_elseif(EL,Env,Env1,Val1),
    boolval(Val1,false),
    eval_elseif(E,Env1,EnvF,Val).

%single elseif block (true)
eval_elseif(t_else_if(SE,S),Env,EnvF,true) :- 
	eval_simple_expr(SE,Env,Env1,Val),
    boolval(Val,true),
    eval_stmt(S,Env1,EnvF).
%single elseif block (false)
eval_elseif(t_else_if(SE,_S),Env,EnvF,false) :- 
	eval_simple_expr(SE,Env,EnvF,Val),
    boolval(Val,false).

% Evaluators for while iteration statements 
eval_iter_stmt(t_while_stmt(SE,_S), Env, EnvF) :- 
    eval_simple_expr(SE,Env,EnvF,Val), boolval(Val, false).
eval_iter_stmt(t_while_stmt(SE,S), Env, EnvF) :- 
    eval_simple_expr(SE,Env,Env1,Val), boolval(Val, true), 
    eval_stmt(S,Env1,Env2), 
    eval_iter_stmt(t_while_stmt(SE,S), Env2, EnvF).


% Evaluators for 'for' iteration statements
% for (; i < 10;)
eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2), _S), Env, EnvF) :-
    eval_mutable_id(ID2, Idname), lookup(Idname, Env, int, _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, EnvF, Val), boolval(Val, false).

% for (; i < 10;)
eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2), S), Env, EnvF) :-
    eval_mutable_id(ID2, Idname), lookup(Idname, Env, int, _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, Env1, Val), boolval(Val, true),
    eval_stmt(S, Env1, Env2),
    eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2), S), Env2, EnvF).

%for (i = 0; i < 10;)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,ID2,RO,SE2),_S), Env, EnvF) :-
    eval_simple_expr(SE1, Env, Env1, Val1), eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env1, int, _),
    update(Idname, int, Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, EnvF, Val2), boolval(Val2, false).

%for (i = 0; i < 10;)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,ID2,RO,SE2),S), Env, EnvF) :-
	eval_simple_expr(SE1, Env, Env1, Val1), eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env1, int, _),
    update(Idname, int, Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, Env3, Val2), boolval(Val2, true),
    eval_stmt(S, Env3, Env4),
    eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2), S), Env4, EnvF).

% for (i < 10; i++)
eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2,_E), _S), Env, EnvF) :-
    eval_mutable_id(ID2, Idname), lookup(Idname, Env, int, _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, EnvF, Val), boolval(Val, false).

% for (i < 10; i++)
eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2,E), S), Env, EnvF) :-
    eval_mutable_id(ID2, Idname), lookup(Idname, Env, int, _),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env, Env1, Val), boolval(Val, true),
    eval_stmt(S, Env1, Env2),
    eval_expr(E, Env2, Env3, _Val),
    eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2,E), S), Env3, EnvF).

% for (i = 0; i < 10; i++)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,ID2,RO,SE2,_E),_S), Env, EnvF) :-
    eval_simple_expr(SE1, Env, Env1, Val1), eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env1, int, _),
    update(Idname, int, Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, EnvF, Val2), boolval(Val2, false).

% for (i = 0; i < 10; i++)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,ID2,RO,SE2,E),S), Env, EnvF) :-
	eval_simple_expr(SE1, Env, Env1, Val1), eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env1, int, _),
    update(Idname, int, Val1, Env1, Env2),
    eval_rel_expr(t_rel_expr(ID2,RO,SE2), Env2, Env3, Val2), boolval(Val2, true),
    eval_stmt(S, Env3, Env4),
    eval_expr(E, Env4, Env5, _Val),
    eval_iter_stmt(t_for_stmt(t_iter_range(ID2,RO,SE2,E), S), Env5, EnvF).



% for in range (1, 10)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,SE2),_S), Env, EnvF) :-
	eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env, int, _),
    eval_simple_expr(SE1, Env, Env1, Val1),
    update(Idname, int, Val1, Env1, Env2),
    eval_simple_expr(SE2, Env2, EnvF, Val2),
    Val1 >= Val2.
    
% for in range (1, 10)
eval_iter_stmt(t_for_stmt(t_iter_range(ID1,SE1,SE2),S), Env, EnvF) :-
	eval_mutable_id(ID1, Idname), 
    lookup(Idname, Env, int, _),
    eval_simple_expr(SE1, Env, Env1, Val1),
    update(Idname, int, Val1, Env1, Env2),
    eval_simple_expr(SE2, Env2, Env3, Val2),
    Val1 < Val2, 
    eval_stmt(S, Env3, Env4),
    eval_expr(t_increment(ID1), Env4, Env5, _),
    eval_iter_stmt(t_for_stmt(t_iter_range(ID1,t_less_than_op(<),SE2,t_increment(ID1)), S), Env5, EnvF).







% Evaluator for assignment expression
eval_expr(t_assignment(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, Type , _),
    update(Idname, Type , Val, Env1, EnvF).

% Evatuator for '+=' Expression 
eval_expr(t_addassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, int , Val1),
    %number(Val),
    Val2 is Val1 + Val,
    update(Idname, int, Val2, Env1, EnvF).

/*
eval_expr(t_addassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, string , Val1),
    Val2 is Val1 + Val,
    update(Idname, string , Val2, Env1, EnvF).
**/

% Evatuator for '-=' Expression
eval_expr(t_subassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, int , Val1),
    Val2 is Val1 - Val,
    update(Idname, int , Val2, Env1, EnvF).

% Evatuator for '*=' Expression
eval_expr(t_multassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, int  , Val1),
    Val2 is Val1 * Val,
    update(Idname, int  , Val2, Env1, EnvF).

% Evatuator for '/=' Expression
eval_expr(t_divassign(M,E), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname), eval_expr(E, Env, Env1, Val),
    lookup(Idname, Env1, int  , Val1),
    Val2 is Val1 / Val,
    update(Idname, int  , Val2, Env1, EnvF).

% Evaluator for increment expression
eval_expr(t_increment(M), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname),
    lookup(Idname, Env, int , Val),
    Val1 is Val + 1,
    update(Idname, int , Val1, Env, EnvF).

% Evaluator for decrement expression
eval_expr(t_decrement(M), Env, EnvF, Val) :- 
    eval_mutable_id(M, Idname),
    lookup(Idname, Env, int , Val),
    Val1 is Val - 1,
    update(Idname, int , Val1, Env, EnvF).
%eval_expr(SE, Env, EnvF, Val) :- eval_simple_expr(SE, Env, EnvF, Val).

eval_expr(TE, Env, EnvF, Val) :- eval_tern_expr(TE, Env, EnvF, Val).

eval_tern_expr(t_tern_expr(SE,E1,_E2),Env,EnvF,Val) :-  
    eval_simple_expr(SE,Env,Env1,Val1),
    boolval(Val1,true),
    eval_expr(E1,Env1,EnvF,Val).

eval_tern_expr(t_tern_expr(SE,_E1,E2),Env,EnvF,Val) :-  
    eval_simple_expr(SE,Env,Env1,Val1),
    boolval(Val1,false),
    eval_expr(E2,Env1,EnvF,Val).

eval_tern_expr(SE, Env, EnvF, Val) :- eval_simple_expr(SE, Env, EnvF, Val).

% Evaluators for or expression
eval_simple_expr(t_or_expr(SE,AE), Env, Env, Val) :-
    eval_simple_expr(SE,Env, Env, Val1), 
    boolval(Val1, Val2),
    eval_and_expr(AE,Env, Env, Val3), 
    Val is Val2 + Val3.
eval_simple_expr(AE , Env, Env, Val) :-
    eval_and_expr(AE, Env, Env, Val).

% Evaluators for and expression
eval_and_expr(t_and_expr(SE,AE), Env, Env, Val) :-
    eval_and_expr(SE,Env, Env, Val1),
    eval_unary_rel_expr(AE,Env, Env, Val2),
    boolval(Val2, Val3),
    Val is Val1 , Val3.
eval_and_expr(AE , Env, Env, Val) :-
    eval_unary_rel_expr(AE, Env, Env, Val).

% Evaluators for unary relational expressions
eval_unary_rel_expr(t_not_expr(URL),Env, EnvF, Val) :-
    eval_unary_rel_expr(URL, Env, EnvF, Val1),
    boolval(Val1,Val2),
    not(Val2, Val). 
eval_unary_rel_expr(RL ,Env, EnvF, Val) :-
    eval_rel_expr(RL, Env, EnvF, Val).

% Evaluators for relational expressions
eval_rel_expr(t_rel_expr(SE1,RO,SE2),Env,EnvF,Val):-
    eval_sum_expr(SE1,Env,Env1,Val1),
    eval_sum_expr(SE2,Env1,EnvF,Val2),
    eval_rel_op(RO,Val1,Val2,Val).
eval_rel_expr(SE,Env,EnvF,Val):-
     eval_sum_expr(SE,Env,EnvF,Val).

% Relational Operator Evaluations
% Evaluator for less than equal to operator
eval_rel_op(t_less_than_eq_op(<=),V1,V2,true):- V1=<V2.
eval_rel_op(t_less_than_eq_op(<=),V1,V2,false):- V1>V2.

% Evaluator for less than operator
eval_rel_op(t_less_than_op(<),V1,V2,true):- V1<V2.
eval_rel_op(t_less_than_op(<),V1,V2,false):- V1>=V2.

% Evaluator for greater than equal to operator
eval_rel_op(t_greater_than_eq_op(>=),V1,V2,true):- V1>=V2.
eval_rel_op(t_greater_than_eq_op(>=),V1,V2,false):- V1<V2.

% Evaluator for greater than operator
eval_rel_op(t_greater_than_op(>),V1,V2,true):- V1>V2.
eval_rel_op(t_greater_than_op(>),V1,V2,false):- V1=<V2.

% Evaluator for equal equal to operator
eval_rel_op(t_eq_op(==),V1,V2,true):- V1==V2.
eval_rel_op(t_eq_op(==),V1,V2,false):- V1=\=V2.

% Evaluator for not equal to operator
eval_rel_op(t_not_eq_op('!='),V1,V2,true):- V1=\=V2.
eval_rel_op(t_not_eq_op('!='),V1,V2,false):- V1==V2.

% Evaluators for sum expressions
eval_sum_expr(t_sum_expr(SE,SO,ME),Env,EnvF,Val):-
    eval_sum_expr(SE,Env,Env1,Val1),
    eval_mult_expr(ME,Env1,EnvF,Val2),
    eval_sum_op(SO,Val1,Val2,Val).
eval_sum_expr(ME,Env,EnvF,Val):-
     eval_mult_expr(ME,Env,EnvF,Val).

% Evaluators for sum operators
eval_sum_op(t_add_op(+),Val1,Val2,Val):-
    Val is Val1+Val2.
eval_sum_op(t_sub_op(-),Val1,Val2,Val):-
    Val is Val1-Val2.

% Evaluators for multiplication expressions
eval_mult_expr(t_mult_expr(ME,MO,UE),Env,EnvF,Val):-
    eval_mult_expr(ME,Env,Env1,Val1),
    eval_unary_expr(UE,Env1,EnvF,Val2),
    eval_mult_op(MO,Val1,Val2,Val).
eval_mult_expr(UE,Env,EnvF,Val):-
     eval_unary_expr(UE,Env,EnvF,Val).

% Evaluators for multiplication operators
eval_mult_op(t_mult_op(*),Val1,Val2,Val):-
    Val is Val1*Val2.
eval_mult_op(t_div_op(/),Val1,Val2,Val):-
    Val is Val1/Val2.
eval_mult_op(t_mod_op('%'),Val1,Val2,Val):-
    Val is mod(Val1,Val2).

% Evaluators for unary expressions
eval_unary_expr(t_unary_expr(UO,UE),Env,EnvF,Val):-
    eval_unary_expr(UE,Env,EnvF,Val1),
    eval_unary_op(UO,Val1,Val).
eval_unary_expr(F,Env,EnvF,Val):-eval_factor(F,Env,EnvF,Val).

% Evaluators for unary operators
eval_unary_op(t_unary_op(-),Val1,Val2):-Val2 is (-1)* Val1.
eval_unary_op(t_unary_op(+),Val1,Val1).

% Evaluators for factors
eval_factor(IM, Env,EnvF,Val):- eval_immutable(IM, Env,EnvF,Val).
eval_factor(M,Env,Env, Val):- eval_mutable(M,Env, Env, Val).

% Evaluator for mutable identifier name
eval_mutable_id(t_mutable(M), Val) :- eval_id_name(M,Val).

% Evaluator for mutable identifier value
eval_mutable(t_mutable(M), Env, Env, Val) :- eval_id(M,Env,Env,Val).

% Evaluator for identifier name
eval_id_name(t_id(I),I).

% Evaluator for identifier value
eval_id(t_id(I),Env,Env,Val):- lookup(I,Env,_,Val).

% Evaluators for immutable constants and expressions
eval_immutable(t_eval_expr(E), Env,EnvF,Val):- eval_expr(E, Env,EnvF,Val).
eval_immutable(t_const(X),Env,Env,Val):- eval_const(X,Val).

% Evaluators for numeric, bool and string constants
eval_const(NC,Val):- eval_num_const(NC,Val).
eval_const(t_bool_const(true),true).
eval_const(t_bool_const(false),false).
eval_num_const(t_num_const(X),X).

% Not predicate for bool type
not(true, false).
not(false, true).

% Predicate for returning bool type for respective value in other data types
boolval(0,false).
boolval('',false).
boolval(false,false).
boolval(Val,true) :- Val \= 0, Val \= '', Val \= false. 

type_check(true,bool).
type_check(false,bool).
type_check(I,int) :- number(I).
type_check(S,string) :- atom_string(A,S), atom(A).

% Predicate to return default data type value
type_default(int,0).
type_default(char,'').
type_default(bool,false).