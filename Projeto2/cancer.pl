:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(random)).

teste :-
	horizontalWall(HP),
	verticalWall(VP),
	verticalNumbers(V),
	horizontalNumbers(H),
	length(V, NRows),
	length(H, NCols),
	doubleCrossAPix(HP, VP, NRows, NCols, V, H).

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

var_table(0, _, []) :- !.

var_table(M, N, T) :-
	M > 0, !,
	length(Row, N),
	M1 is M - 1,
	var_table(M1, N, T1),
	append([Row], T1, T).

doubleCrossAPix(HP, VP, T, V, H) :-
	doubleCrossAPixSolver(HP, VP, T, V, H),
	print_solution(T).
	
doubleCrossAPix(HP, VP, NRows, NCols, V, H) :-
	var_table(NRows, NCols, T),
	doubleCrossAPix(HP, VP, T, V, H).

doubleCrossAPixSolver(HP, VP, T, V, H) :-	
	append(T, Vars),
	domain(Vars, 0, 1),
	groupsWithSameColour(HP,VP,T),
	horizontalRule(T,H),
	verticalRule(T,V),
	labeling([ff,enum],Vars).

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

/* Imprimir solucao em formato legivel */

print_solution(T) :- write('Solution:'), nl, nl, print_solution_aux(T), nl.
print_solution_aux([]).
print_solution_aux([Row|Rows]) :- print_row(Row), nl, print_solution_aux(Rows).

print_row([]).
print_row([0|Row]) :- write('  '), print_row(Row).
print_row([1|Row]) :- write('+ '), print_row(Row).

/* Gerar um tabuleiro aleatorio */

/* Gerar uma matriz de informacoes referentes as paredes de tamanho M*N */
generate_walls(0,_,[]) :- !.
generate_walls(M,N,[Row|T]) :- M > 0, !, generate_line_walls(N,Row), M1 is M-1, generate_walls(M1,N,T).
generate_line_walls(0, []) :- !.
generate_line_walls(N,[Elem|Row]) :- N > 0, !, N1 is N-1, random(0,2,Elem), generate_line_walls(N1, Row).

/* Gerar as duas matrizes com informacao referente aos quadrados pintados / seccoes */
get_horizontal_numbers([], []).
get_horizontal_numbers([Row|Rows], [[PaintedSquares, NumberSections]|NumbersRows]) :- 
	get_row_numbers(Row, PaintedSquares),
	get_row_sections(Row, NumberSections),
	get_horizontal_numbers(Rows, NumbersRows).

get_row_numbers(Row, PaintedSquares) :-
	sumlist(Row, PaintedSquares).

get_row_sections([Elem|Row], NumberSections) :-
	get_row_sections_aux(Row, Elem, Elem, NumberSections).
get_row_sections_aux([], _, N, N).
get_row_sections_aux([1|Row], 0, CurrentNumberSections, NumberSections) :- 
	N is CurrentNumberSections + 1,
	get_row_sections_aux(Row, 1, N, NumberSections). 
get_row_sections_aux([Elem|Row], _,CurrentNumberSections, NumberSections) :- 
	get_row_sections_aux(Row, Elem, CurrentNumberSections, NumberSections).

get_vertical_numbers(Rows, Rules) :-
	transpose(Rows, Cols), get_horizontal_numbers(Cols, Rules).

/* Gerar um jogo M por N */
game_generator(M, N, HP, VP, H, V) :-
	generate_walls(M, N, HP, VP),
	var_table(M, N, T),
	paint_board(HP, VP, T),
	calculateRules(T, H, V).
	
generate_walls(M, N, HP, VP) :-
	N1 is N - 1,
	generate_walls(M, N1, HP),
	generate_walls(M, N1, VP).
	
paint_board(HP, VP, T) :-
	append(T, Vars),
	domain(Vars, 0, 1),
	groupsWithSameColour(HP,VP,T),
	restrainRandomPositions(T),
	labeling([ff,enum],Vars).
	
calculateRules(T, H, V) :-
	get_horizontal_numbers(T, H),
	get_vertical_numbers(T,V).
	
restrainRandomPositions(T) :-
	getBoardSize(T, NumRows, NumCols),
	MinElems is truncate(sqrt(min(NumRows, NumCols))),
	MaxElems is truncate(sqrt(NumRows * NumCols)) + 1,
	random(MinElems, MaxElems, NumElemsToPaint),
	getRandomPositions(NumElemsToPaint, NumRows, NumCols, Positions),
	restrainElems(Positions, T).
	
getBoardSize(T, NumRows, NumCols) :-
	length(T, NumRows),
	nth0(0, T, Row),
	length(Row, NumCols).
	
getRandomPositions(NumPositions, NumRows, NumCols, Positions) :-
	getRandomPositionsAux(NumPositions, NumRows, NumCols, PositionsList),
	remove_dups(PositionsList, Positions).
getRandomPositionsAux(0, _,_,[]) :- !.
getRandomPositionsAux(NumPositions, NumRows, NumCols, [[Row,Col]|Positions]) :-
	NumPositions > 0, !, 
	random(0, NumRows, Row),
	random(0, NumCols, Col),
	X is NumPositions - 1,
	getRandomPositions(X, NumRows, NumCols, Positions).
	
restrainElems([], _).
restrainElems([[Row,Col]|Positions], T) :-
	nth0(Row, T, TRow),
	nth0(Col, TRow, Elem),
	Elem #= 1,
	restrainElems(Positions, T).