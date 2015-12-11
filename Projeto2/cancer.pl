:- use_module(library(clpfd)).
:- use_module(library(lists)).

doubleCrossAPix(HP, VP, T, V, H) :-
	append(T, Var),
	domain(Var, 0, 1),
	groupsWithSameColour(HP,VP,T),
	labeling([],Var).

/* Para cada elemento pertencente ao mesmo bloco, a sua cor tem de ser igual  */

groupsWithSameColour(HP, VP, T) :-
	groupsWithSameColourHorizontal(HP, T),
	groupsWithSameColourVertical(VP, T).



get_colour(T, Row, Col, Colour) :- nth0(Row, T, Elem), nth0(Col, Elem, Colour).

/* O numero de quadrados pretos da coluna C tem de ser igual a V[C][0]
	O numero de blocos pretos da coluna C tem de ser igual a V[C][1]  */

horizontalRule([], []).
horizontalRule([Row|Rows],[Rule|Rules]) :-
	validate_row(Row, Rule), horizontalRule([Rows|Rules]).

validate_row(Row, [PaintedSquares|NumberSections])
	check_row_painted_squares(Row, PaintedSquares),
	check_row_sections(Row, NumberSections).

check_row_painted_squares([], 0).
check_row_painted_squares([Elem|Row], PaintedSquares) :- 
	Elem #= 0,
	check_row_painted_squares(Row, PaintedSquares).
check_row_painted_squares([Elem|Row], PaintedSquares) :- 
	Elem #= 1,
	N is PaintedSquares - 1, check_row_painted_squares(Row, N).

check_row_sections([Elem|Row], NumberSections) :- Elem #= 0,
	check_row_sections_aux(Row, 0, 0, NumberSections).
check_row_sections([Elem|Row], NumberSections) :- Elem #= 1,
	check_row_sections_aux(Row, 1, 1, NumberSections).

check_row_sections_aux([], N, N).
check_row_sections_aux([Elem|Row], LastElem, CurrentNumberSections, NumberSections) :- 
	Elem #= 1,
	LastElem #= 0,
	!,
	N is CurrentNumberSections + 1,
	check_row_sections_aux(Row, Elem, N, NumberSections). 
check_row_sections_aux([Elem|Row], LastElem,CurrentNumberSections, NumberSections) :- 
	check_row_sections_aux(Row, Elem, CurrentNumberSections, NumberSections).

/* 	O numero de quadrados pretos da linha L tem de ser igual a V[H][0]
	O numero de blocos pretos da linha L tem de ser igual a V[H][1] */

