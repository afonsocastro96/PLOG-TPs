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

teste_print :-
	horizontalWall(HP),
	verticalWall(VP),
	verticalNumbers(V),
	horizontalNumbers(H),
	print_white_puzzle(HP, VP, H, V).
	
test_generator(M, N) :-
	game_generator(M, N, HP, VP, H, V),
	print_white_puzzle(HP, VP, H, V).
	
test_generate_and_solve(M, N) :-
	game_generator(M, N, HP, VP, H, V),
	print_white_puzzle(HP, VP, H, V),
	doubleCrossAPix(HP, VP, M, N, V, H).
	
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
	print_puzzle(HP, VP, T, H, V) .
	
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
print_white_puzzle(HP, VP, H, V) :-
	length(H, NumRows),
	length(V, NumCols),
	white_board(NumRows, NumCols, T),
	print_puzzle(HP, VP, T, H, V).

white_board(0, _, []).
white_board(NumRows, NumCols, [Row|Rows]) :-
	NumRows > 0,
	white_row(NumCols, Row),
	X is NumRows - 1,
	white_board(X, NumCols, Rows).
	
white_row(0, []).
white_row(NumCols, [0|Cols]) :-
	NumCols > 0,
	X is NumCols - 1,
	white_row(X, Cols).

print_puzzle(HP, VP, T, H, V) :-
	getSpacing(H, PaintedSpace, SectionsSpace),
	Spacing is PaintedSpace + 1 + SectionsSpace,
	length(V, NumCols),
	print_vertical_rules(V, Spacing),
	transpose(VP, VPT),
	print_horizontal_border(Spacing, NumCols),
	print_puzzle_aux(HP, VPT, T, H, PaintedSpace, SectionsSpace, Spacing).

print_vertical_rules(V, Spacing) :-
	getSpacing(V, PaintedSpace, SectionsSpace),
	transpose(V, [Painted, Sections]),
	print_vertical_numbers(Spacing, Painted, PaintedSpace),
	length(Painted, NumCols),
	print_division(Spacing, NumCols),
	print_vertical_numbers(Spacing, Sections, SectionsSpace).
	
print_vertical_numbers(_, _, 0).
	
print_vertical_numbers(Spacing, Numbers, NumbersSpace) :-
	print_spacing(Spacing),
	N is NumbersSpace - 1,
	print_vertical_numbers_aux(Numbers, N, Remainders),
	print_vertical_numbers(Spacing, Remainders, N).
	
print_vertical_numbers_aux([], _, []) :-
	write('\n').
print_vertical_numbers_aux([Number|Numbers], N, [Remainder|Remainders]) :-
	Digit is Number // 10 ^ N,
	Remainder is Number rem 10 ^ N,
	write(' '),
	write(Digit),
	print_vertical_numbers_aux(Numbers, N, Remainders).
	
print_division(Spacing, NumCols) :-
	print_spacing(Spacing),
	print_division_aux(NumCols).
	
print_division_aux(0) :-
	write('\n').
print_division_aux(NumCols) :-
	N is NumCols - 1,
	write(' -'),
	print_division_aux(N).
	
print_horizontal_border(Spacing, NumCols) :-	
	print_spacing(Spacing),
	print_line(NumCols),
	write('\n').
	
getSpacing(H, PaintedSpace, SectionsSpace) :-
	transpose(H, [Painted, Sections]),
	max_member(MaxPainted, Painted),
	PaintedSpace is truncate(log(10,MaxPainted)) + 1,
	max_member(MaxSections, Sections),
	SectionsSpace is truncate(log(10,MaxSections)) + 1.
	
print_spacing(0).
print_spacing(Spacing) :-
	X is Spacing - 1,
	write(' '),
	print_spacing(X).

print_line(0) :-
	write('+').
print_line(NumCols) :-
	X is NumCols - 1,
	write('+-'),
	print_line(X).	

	
print_puzzle_aux([HWall], [], [Row], [HRule], PaintedSpace, SectionsSpace, Spacing) :-
	print_horizontal_rule(HRule, PaintedSpace, SectionsSpace),
	print_row(HWall, Row),
	write('\n'),
	length(Row, NumCols),
	print_horizontal_border(Spacing, NumCols),
	write('\n').

print_puzzle_aux([HWall|HWalls], [VWall|VWalls], [Row|Rows], [HRule|HRules], PaintedSpace, SectionsSpace, Spacing) :-
	print_horizontal_rule(HRule, PaintedSpace, SectionsSpace),
	print_row(HWall, Row),
	write('\n'),
	print_vertical_walls(VWall, Spacing),
	write('\n'),
	print_puzzle_aux(HWalls, VWalls, Rows, HRules, PaintedSpace, SectionsSpace, Spacing).

print_horizontal_rule([Painted,Sections], PaintedSpace, SectionsSpace) :-
	print_number_horizontal(Painted, PaintedSpace),
	write('|'),
	print_number_horizontal(Sections, SectionsSpace).

print_number_horizontal(_, 0).
print_number_horizontal(Number, Space) :-
	N is Space - 1,
	Digit is Number // 10 ^ N,
	Remainder is Number rem 10 ^ N,
	write(Digit),
	print_number_horizontal(Remainder, N).

print_row(Walls, Elems) :-
	write('|'),
	print_row_aux(Walls, Elems).
	
print_row_aux([], [Elem]) :-
	print_elem(Elem),
	write('|').

print_row_aux([Wall|Walls], [Elem|Elems]) :-
	print_elem(Elem),
	print_horizontal_wall(Wall),
	print_row_aux(Walls, Elems).

print_elem(0) :-
	write(' ').
print_elem(1) :-
	write('*').

print_horizontal_wall(0) :-
	write(' ').
print_horizontal_wall(1) :-
	write('|').

print_vertical_walls(VWall, Spacing) :-
	print_spacing(Spacing),
	print_vertical_walls_aux(VWall).
	
print_vertical_walls_aux([]) :-
	write('+').
	
print_vertical_walls_aux([VWall|VWalls]) :-
	write('+'),
	print_vertical_wall(VWall),
	print_vertical_walls_aux(VWalls).
	
print_vertical_wall(0) :-
	write(' ').
print_vertical_wall(1) :-
	write('-').

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
	M1 is M - 1,
	N1 is N - 1,
	generate_walls(M, N1, HP),
	generate_walls(N, M1, VP).
	
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