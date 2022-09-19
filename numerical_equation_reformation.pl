
example_EleForce_HLC_2022:-
  Magnitude = 6.7e-11 * Q1 * Q2 / ((X2 - X1)**2 + (Y2 - Y1)**2 + (Z2 - Z1)**2),
  Direction = 1.0 / ((X2 - X1)**2 + (Y2 - Y1)**2 + (Z2 - Z1)**2)**0.5 * [vector, X2 - X1, Y2 - Y1, Z2 - Z1],
  EleForce_Equation = [
    +[is_equal_to, [ele_force, P1, P2], [numerical, Magnitude * Direction]],
    -[charge, P1, Q1],
    -[position, P1, X1, Y1, Z1],
    -[charge, P2, Q2],
    -[position, P2, X2, Y2, Z2]
  ],
  Rules = [
    [+[charge, p1, 4]],
    [+[position, p1, 0.1, 0, 0]],
    [+[charge, p2, 4]],
    [+[position, p2, 0, 0, 0]],
    EleForce_Equation
  ],
  Goal = [-[is_equal_to, [ele_force, p1, p2], [numerical, [vector, 1.44, 0, 0]]]],
  %Goal = [-[is_equal_to, [ele_force, p1, p2], Result]],
  writeln("\n########## PROBLEM ##########\n"),
  writeln("Rules:\n"),
  write_in_lines(Rules),
  writeln("Goals:\n"),
  write_in_lines(Goal),
 
  sld_resolution_z3(Rules, Goal).
  %writeln("Solved Goal:"),
  %write_in_lines(Goal).


% Solve numerical equations using Python Z3.
solve_numerical_equations_by_z3(Goal):-
  rewrite_numerical_equations(Goal, Goal_T),
  %writeln("RESOLVE Numerical Goal:"),
  %write_in_lines(Goal_T),
  convert_numerical_goals_to_string(Goal_T, Goal_String),
  writeln("\nGoal for z3:\n"),
  writeln(Goal_String),
  process_create(path("python3"), ["equation_weakening_z3.py", Goal_String], [stdout(pipe(In))]),
  read_string(In, Len, X),
  term_string([Flag | Repairs], X),
  writeln('\n\n########## RESULT ##########\n'),
  (
    Flag = 'The equation is true. It will not be weakened.',
    writeln('The goal can be proved by z3.'),
    !
    ;
    Flag = 'The equation is false. It will be weakened.',
    writeln('By applying one of the following repairs, the goal can be proved by z3:\n'),
    write_in_lines(Repairs),
    !
  ).

convert_numerical_goals_to_string([X|[]], Result):-
  X = -[numerical_equation, Left_Expression, Right_Expression],
  term_to_atom(Left_Expression, Left_Text),
  term_to_atom(Right_Expression, Right_Text),
  atomic_list_concat([Left_Text, ' == ', Right_Text], Result),
  !.  
convert_numerical_goals_to_string([X|Remaining_Goals], Result):-
  length(Remaining_Goals, N),
  %writeln(X),writeln(Remaining_Goals),
  N >= 1,
  convert_numerical_goals_to_string(Remaining_Goals, Sub_Result),
  X = -[numerical_equation, Left_Expression, Right_Expression],
  term_to_atom(Left_Expression, Left_Text),
  term_to_atom(Right_Expression, Right_Text),
  atomic_list_concat([Left_Text, ' == ', Right_Text, ' <AND> ', Sub_Result], Result),
  !.

rewrite_numerical_equations([], []):-!.

rewrite_numerical_equations([-[numerical_equation, Left_Expression, Right_Expression] | Remaining_Equations], Result):-
  rewrite_expression(Left_Expression, Left_Expression_T),
  rewrite_expression(Right_Expression, Right_Expression_T),
  build_equations(Left_Expression_T, Right_Expression_T, Result_1),
  rewrite_numerical_equations(Remaining_Equations, Result_2),
  append(Result_1, Result_2, Result),
  !.

build_equations([], [], []):-!.

build_equations([vector | Vector_1], [vector | Vector_2], Result):-
  build_equations(Vector_1, Vector_2, Result),
  !.

build_equations([Expression_1 | Remaining_Expressions_1], [Expression_2 | Remaining_Expressions_2], [-Equation | Sub_Result]):-
  Equation = [numerical_equation, Expression_1, Expression_2],
  build_equations(Remaining_Expressions_1, Remaining_Expressions_2, Sub_Result),
  !.

build_equations(Expression_1, Expression_2, [-[numerical_equation, Expression_1, Expression_2]]):-!.

% ###### rewrite rules used by the demo ######

% Iteratively apply rewrite rules to the given expression until it cannot be further written.

rewrite_expression(Expression, Result):-
  (
    number_dot_vector(Expression, Expression_T)
    ;
    associate_law_of_multiplication(Expression, Expression_T)
  ),
  %writeln(Expression_T),
  rewrite_expression(Expression_T, Result),
  !.

rewrite_expression(Expression, Expression):-!.

associate_law_of_multiplication(A * (B * C), (A * B) * C):-!.

number_dot_vector(Number * [vector | Vector], [vector | Result]):-
  number_dot_vector(Number, Vector, Result),
  !.

number_dot_vector(_, [], []):-!.

number_dot_vector(Number, [First_Element | Remaining_Elements], [Number * First_Element | Sub_Result]):-
  number_dot_vector(Number, Remaining_Elements, Sub_Result),
  !.
 
% ###### end of the rewrite rules ###### 

% SLD resolution with Z3 -- sld_resolution_z3(Rules, Goal)

% Successful.
sld_resolution_z3(_, []):-!.

% Solve the goal using the Z3 solver.
sld_resolution_z3(_, Goal):-
  \+has_non_numerical_literals(Goal), % i.e., all literals are numerical equations.
  solve_numerical_equations_by_z3(Goal),
  !.
 
% Postpone the resolution of literals with numerical equations.
sld_resolution_z3(Rules, [-Literal | Sub_Goal]):-
  Literal = [numerical_equation, _, _],
  append(Sub_Goal, [-Literal], New_Goal),
  sld_resolution_z3(Rules, New_Goal),
  !.

% Eliminate a negative literal.
sld_resolution_z3(Rules, Goal):-
  writeln("\n\n######## SLD-Resolution ########\n"),
  writeln("\nRules:\n"),
  write_in_lines(Rules),
  writeln("\nGoal:\n"),
  write_in_lines(Goal),
  [-Literal_1 | Sub_Goal_1] = Goal,
  member(Rule, Rules),
  copy_term(Rule, Free_Rule),
  [+Literal_2 | Sub_Goal_2] = Free_Rule,
  lazy_unification(Literal_1, Literal_2, Sub_Goal_3),
  append(Sub_Goal_1, Sub_Goal_2, Sub_Goal_1_2),
  append(Sub_Goal_1_2, Sub_Goal_3, Sub_Goal),
  sld_resolution_z3(Rules, Sub_Goal).


% Lasy unification that postpones the unification of numerical terms as a sub-goal -- lazy_unification(Term_1, Term_2, Sub_Goal)

% Unification without any numerical terms.
lazy_unification(Term, Term, []):-
  \+has_numerical_term(Term),
  !.

% Considering the unification of numerical terms as a goal of equation solving.
lazy_unification(Term_1, Term_2, [-Equation]):-
  \+var(Term_1),
  \+var(Term_2),
  Term_1 = [numerical, Expression_1],
  Term_2 = [numerical, Expression_2],
  Equation = [numerical_equation, Expression_1, Expression_2],
  !.

% Unification of sub-terms.
lazy_unification([Predicate | Sub_Terms_1], [Predicate | Sub_Terms_2], Goal):-
  lazy_unification_of_sub_terms(Sub_Terms_1, Sub_Terms_2, Goal),
  !.

lazy_unification_of_sub_terms([], [], []):-!.

lazy_unification_of_sub_terms([Term_1 | Remaining_Terms_1], [Term_2 | Remaining_Terms_2], Goal):-
  lazy_unification(Term_1, Term_2, Sub_Goal_A),
  lazy_unification_of_sub_terms(Remaining_Terms_1, Remaining_Terms_2, Sub_Goal_B),
  append(Sub_Goal_A, Sub_Goal_B, Goal),
  !.
  
  

% Check if a term has any numerical terms.

has_numerical_term(Term):-
  var(Term),
  !,
  fail.

has_numerical_term([numerical, _]):-!.

has_numerical_term([_ | Sub_Terms]):-
  has_numerical_sub_term(Sub_Terms),
  !.

has_numerical_sub_term([Sub_Term | _]):-
  has_numerical_term(Sub_Term),
  !.

has_numerical_sub_term([_ | Remaining_Terms]):-
  has_numerical_sub_term(Remaining_Terms),
  !.

% Check if a goal has non-numerical literals.

has_non_numerical_literals([-Literal | _]):-
  Literal = [Predicate | _],
  Predicate \= numerical_equation,
  !.

has_non_numerical_literals([_ | Sub_Goal]):-
  has_non_numerical_literals(Sub_Goal),
  !.


% Plain SLD resolution.

sld_resolution(_, []):-!.

sld_resolution(Rules, Goal):-
  %writeln("Resolve"),
  %writeln(Rules),
  %writeln(Goal),
  [-Literal | Sub_Goal_1] = Goal,
  member(Rule, Rules),
  copy_term(Rule, Free_Rule),
  [+Literal | Sub_Goal_2] = Free_Rule,
  append(Sub_Goal_1, Sub_Goal_2, Sub_Goal),
  sld_resolution(Rules, Sub_Goal).

% goal_success(Goal) returns true if Goal only contains positive literals.

goal_success([]):-!.

goal_success([+First_Literal | Sub_Goal]):-
  goal_success(Sub_Goal),
  !.

%

apply_rule(Rule, Goal, Sub_Goal):-
  %writeln("Apply a rule:"),
  %writeln(Rule),
  %writeln(Goal),
  [+Positive_Literal | Negative_Literals] = Rule,
  append(Sub_Goal_1, [-Positive_Literal | Sub_Goal_2], Goal),
  append(Sub_Goal_1, Negative_Literals, Sub_Goal_3),
  append(Sub_Goal_3, Sub_Goal_2, Sub_Goal),
  %writeln(Sub_Goal),
  !.

write_in_lines([]):-!.
write_in_lines([X|L]):-
  writeln(X),nl,
  write_in_lines(L),
  !.

/*
Steps:
1. Declare 

*/

