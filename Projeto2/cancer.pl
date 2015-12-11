:- use_module(library(clpfd)).
:- use_module(library(lists)).

horizontalWall(Walls) :- Walls = [[0,1,1,0,1,1,0,1,1],
								[0,1,1,1,1,1,1,1,1],
								[1,1,1,1,0,1,1,1,1],
								[0,1,1,1,1,1,0,1,0],
								[1,1,0,1,0,1,0,1,0],
								[0,1,1,1,0,1,1,1,0],
								[1,0,1,1,1,1,0,0,1],
								[1,0,0,1,1,1,1,0,1],
								[1,1,0,1,0,1,0,1,1],
								[1,0,1,1,0,1,1,1,0]].

verticalWall(Walls) :- Walls =   [[0,1,1,1,1,1,0,1,0],
								[0,1,1,1,1,1,1,1,1],
								[0,1,0,1,0,1,1,1,1],
								[1,0,1,0,1,1,1,1,0],
								[1,1,1,0,1,1,1,1,1],
								[1,1,1,1,1,0,1,1,1],
								[0,0,1,0,1,1,0,1,1],
								[1,0,1,0,1,1,1,1,0],
								[1,0,1,1,1,1,1,0,1],
								[1,1,0,1,1,1,0,0,1]].

verticalNumbers(Numbers) :- Numbers = [ [2,1],
										[2,2],
										[4,3],
										[5,3],
										[4,2],
										[4,2],
										[6,1],
										[7,3],
										[5,4],
										[4,3]].

horizontalNumbers(Numbers) :- Numbers = [	[3,3],
											[4,3],
											[5,3],
											[5,2],
											[4,1],
											[4,3],
											[7,3],
											[2,2],
											[5,2],
											[4,2]].									

var_table(0, _, []).

var_table(M, N, T) :-
	length(Row, N),
	M1 is M - 1,
	var_table(M1, N, T1),
	append([Row], T1, T).

doubleCrossAPix(HP, VP, T, V, H) :-
	append(T, Var),
	domain(Var, 0, 1),
	groupsWithSameColour(HP,VP,T),
	horizontalRule(T,H),
	verticalRule(T,V),
	labeling([ff,enum],Var).

/* Para cada elemento pertencente ao mesmo bloco, a sua cor tem de ser igual  */

groupsWithSameColour(HP, VP, T) :-
	groupsWithSameColourHorizontal(HP, T),
	groupsWithSameColourVertical(VP, T).

groupsWithSameColourVertical(VP, T) :-
	transpose(T, TT),
	groupsWithSameColourHorizontal(VP, TT).
	
groupsWithSameColourHorizontal([],[]).
groupsWithSameColourHorizontal([HRowWalls|HRowsWalls],[Row|Rows]) :-
	checkWalls(HRowWalls, Row),
	groupsWithSameColourHorizontal(HRowsWalls, Rows).
	
checkWalls([], [_]). /* Last elem */
checkWalls([HWall|HWalls], [Elem1,Elem2|Elems]) :-
	checkWall(HWall, Elem1, Elem2),
	checkWalls(HWalls, [Elem2|Elems]).

checkWall(0, Elem1, Elem2) :-
	Elem1 #= Elem2.
checkWall(1, _, _).

get_colour(T, Row, Col, Colour) :- nth0(Row, T, Elem), nth0(Col, Elem, Colour).

/* O numero de quadrados pretos da coluna C tem de ser igual a V[C][0]
	O numero de blocos pretos da coluna C tem de ser igual a V[C][1]  */

horizontalRule([], []).
horizontalRule([Row|Rows],[Rule|Rules]) :-
	validate_row(Row, Rule), horizontalRule(Rows,Rules).

validate_row(Row, [PaintedSquares,NumberSections]) :-
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

check_row_sections_aux([], _, N, N).
check_row_sections_aux([Elem|Row], LastElem, CurrentNumberSections, NumberSections) :- 
	Elem #= 1,
	LastElem #= 0,
	!,
	N is CurrentNumberSections + 1,
	check_row_sections_aux(Row, Elem, N, NumberSections). 
check_row_sections_aux([Elem|Row], _,CurrentNumberSections, NumberSections) :- 
	check_row_sections_aux(Row, Elem, CurrentNumberSections, NumberSections).

/* 	O numero de quadrados pretos da linha L tem de ser igual a V[H][0]
	O numero de blocos pretos da linha L tem de ser igual a V[H][1] */

verticalRule(Rows, Rules) :-
	transpose(Rows, Cols), horizontalRule(Cols, Rules).