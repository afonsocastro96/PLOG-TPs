%MODULES
:- use_module(library(random)).

major_board(Board) :- Board =
[	[[' ',' ',' '], [' ',' ',' '],[' ',' ',' '], [' ',' ',' '],
	 [' ',' ',' '], [' ',' ',' '],[' ',' ',' ']],
	[[' ',' ',' '], [' ',' ',' '],[' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']],
	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']],
	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' '], [' ',' ',' '], [' ',' ',' ']]
].
 
minor_board(Board) :- Board =
	[[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
	 [' ',' ',' ']],
	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
	 [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' ']],
 	[[' ',' ',' '], [' ',' ',' '], [' ',' ',' '], [' ',' ',' '],
 	 [' ',' ',' ']]].

display_board(Board) :- write_col_coords(Board), display_board_aux(Board,1).

display_board_aux([Line | Board],Number) :- Board \= [], write_border(Line),
write_line(Line, Number), NextNumber is Number + 1,
display_board_aux(Board,NextNumber).

display_board_aux([Line], Number) :-write_border(Line),
write_line(Line, Number), write_border(Line).

write_border([_|Line]) :- write('   +---'), write_border_aux(Line).

write_border_aux([]) :- write('+\n').

write_border_aux([_|Line]) :- write('+---'), write_border_aux(Line).

write_aux_line([]) :- write('|\n').

write_aux_line([Elem|Line]) :- write('|'), write_elem(Elem),
write_aux_line(Line).

write_line(Line, Number) :- write(' '), write(Number), write(' '),
write_aux_line(Line).

write_elem([Tower,Colour,Shape]) :- write(Tower), write(Colour), write(Shape).

write_col_coords([Line | _]) :- write('   '),
write_col_coords_aux(Line,65).

write_col_coords_aux([_],Charcode) :- char_code(Character,Charcode),
write('  '), write(Character), write('\n').

write_col_coords_aux([_|Line],Charcode) :- Line \= [],
char_code(Character,Charcode), write('  '), write(Character),
write(' '), Nextchar is Charcode+1, write_col_coords_aux(Line,Nextchar). 

% Return L[X][Y]
return_element(L,X,Y,Elem) :- return_element_aux_x(L,X,Y,Elem).
return_element_aux_x([H|_],0,Y,Elem) :- return_element_aux_y(H,Y,Elem).
return_element_aux_x([_|T],X,Y,Elem) :- X>0,Z is X-1, return_element_aux_x(T,Z,Y,Elem).
return_element_aux_y([H|_],0,H). 
return_element_aux_y([_|T],Y,Elem) :- Y>0,Z is Y-1, return_element_aux_y(T,Z,Elem).

%Replace L[X][Y] with NElem
replace_element(L,X,Y,NElem,NL) :- replace_element_aux_x(L,X,Y,NElem,NL).
replace_element_aux_x([H|T],0,Y,NElem,[NH|T]) :- replace_element_aux_y(H,Y,NElem, NH).
replace_element_aux_x([H|L],X,Y,NElem,[H|NL]) :- X>0,Z is X-1, replace_element_aux_x(L,Z,Y,NElem,NL).
replace_element_aux_y([_|T],0,NElem,[NElem|T]).
replace_element_aux_y([H|L],Y,NElem,[H|NL]) :- Y>0,Z is Y-1, replace_element_aux_y(L,Z,NElem,NL).

%Insert tower into tile (T-> Black/square tower L->White/circle tower)
insert_tower(Board, X, Y, Tower,NBoard) :- insert_tower_aux_x(Board, X, Y, Tower, NBoard).
insert_tower_aux_x([H|T],0,Y,Tower,[NH|T]) :- insert_tower_aux_y(H,Y,Tower, NH).
insert_tower_aux_x([H|L],X,Y,Tower,[H|NL]) :- X>0,Z is X-1, insert_tower_aux_x(L,Z,Y,Tower,NL).
insert_tower_aux_y([H|T],0,Tower,[NElem|T]) :- insert_tower_into_place(H,Tower,NElem).
insert_tower_aux_y([H|L],Y,Tower,[H|NL]) :- Y>0,Z is Y-1, insert_tower_aux_y(L,Z,Tower,NL).
insert_tower_into_place([_|['P',B]],'T',['T'|['P',B]]).
insert_tower_into_place([_|[A,'Q']],'T',['T'|[A,'Q']]).
insert_tower_into_place([_|['B',B]],'L',['L'|['B',B]]).
insert_tower_into_place([_|[A,'C']],'L',['L'|[A,'C']]).

%Remove tower

remove_tower(Board, X, Y,NBoard) :- remove_tower_aux_x(Board, X, Y, NBoard).
remove_tower_aux_x([H|T],0,Y,[NH|T]) :- remove_tower_aux_y(H,Y, NH).
remove_tower_aux_x([H|L],X,Y,[H|NL]) :- X>0,Z is X-1, remove_tower_aux_x(L,Z,Y,NL).
remove_tower_aux_y([H|T],0,[NElem|T]) :- remove_tower_from_place(H,NElem).
remove_tower_aux_y([H|L],Y,Tower,[H|NL]) :- Y>0,Z is Y-1, remove_tower_aux_y(L,Z,NL).
remove_tower_from_place([_|T],[' '|T]).



%Start game
start_game :- write('Please state the board you want (major/minor): '), read(X), create_board(_,X).
create_board(Board, minor) :- minor_board(Board), randomize_board_minor(Board).
create_board(Board, major) :- major_board(Board), randomize_board_major(Board).

%Randomize board
randomize(N) :- random(0,3,N).
	
randomize_board_major(Board) :- randomize(N), replace_board(Board,3,2,3,4,N, NBoard), randomize_board_major_3(NBoard).
randomize_board_major_3(Board) :- randomize(N), replace_board(Board,3,1,3,5,N, NBoard), randomize_board_major_5(NBoard).
randomize_board_major_5(Board) :- randomize(N), replace_board(Board,3,0,3,6,N, NBoard), randomize_board_major_7(NBoard).
randomize_board_major_7(Board) :- randomize(N), replace_board(Board,2,0,4,6,N, NBoard), randomize_board_major_9(NBoard).
randomize_board_major_9(Board) :- randomize(N), replace_board(Board,2,1,4,5,N, NBoard), randomize_board_major_11(NBoard).
randomize_board_major_11(Board) :- randomize(N), replace_board(Board,2,2,4,4,N, NBoard), randomize_board_major_13(NBoard).
randomize_board_major_13(Board) :- randomize(N), replace_board(Board,2,3,4,3,N, NBoard), randomize_board_major_15(NBoard).
randomize_board_major_15(Board) :- randomize(N), replace_board(Board,2,4,4,2,N, NBoard), randomize_board_major_17(NBoard).
randomize_board_major_17(Board) :- randomize(N), replace_board(Board,2,5,4,1,N, NBoard), randomize_board_major_19(NBoard).
randomize_board_major_19(Board) :- randomize(N), replace_board(Board,2,6,4,0,N, NBoard), randomize_board_major_21(NBoard).
randomize_board_major_21(Board) :- randomize(N), replace_board(Board,1,5,5,1,N, NBoard), randomize_board_major_23(NBoard).
randomize_board_major_23(Board) :- randomize(N), replace_board(Board,1,4,5,2,N, NBoard), randomize_board_major_25(NBoard).
randomize_board_major_25(Board) :- randomize(N), replace_board(Board,1,3,5,3,N, NBoard), randomize_board_major_27(NBoard).
randomize_board_major_27(Board) :- randomize(N), replace_board(Board,1,2,5,4,N, NBoard), randomize_board_major_29(NBoard).
randomize_board_major_29(Board) :- randomize(N), replace_board(Board,1,1,5,5,N, NBoard), randomize_board_major_31(NBoard).
randomize_board_major_31(Board) :- randomize(N), replace_board(Board,0,2,6,4,N, NBoard), randomize_board_major_33(NBoard).
randomize_board_major_33(Board) :- randomize(N), replace_board(Board,0,3,6,3,N, NBoard), randomize_board_major_35(NBoard).
randomize_board_major_35(Board) :- randomize(N), replace_board(Board,0,4,6,2,N, NBoard), pick_tower(NBoard).

randomize_board_minor(Board) :- randomize(N), replace_board(Board,2,1,2,3,N, NBoard), randomize_board_minor_3(NBoard).
randomize_board_minor_3(Board) :- randomize(N), replace_board(Board,2,0,2,4,N, NBoard), randomize_board_minor_5(NBoard).
randomize_board_minor_5(Board) :- randomize(N), replace_board(Board,1,0,3,4,N, NBoard), randomize_board_minor_7(NBoard).
randomize_board_minor_7(Board) :- randomize(N), replace_board(Board,1,1,3,3,N, NBoard), randomize_board_minor_9(NBoard).
randomize_board_minor_9(Board) :- randomize(N), replace_board(Board,1,2,3,2,N, NBoard), randomize_board_minor_11(NBoard).
randomize_board_minor_11(Board) :- randomize(N), replace_board(Board,1,3,3,1,N, NBoard), randomize_board_minor_13(NBoard).
randomize_board_minor_13(Board) :- randomize(N), replace_board(Board,0,3,4,1,N, NBoard), randomize_board_minor_15(NBoard).
randomize_board_minor_15(Board) :- randomize(N), replace_board(Board,0,2,4,2,N, NBoard), pick_tower(NBoard).

% 0-BC 1-PC 2-BQ 3-PQ
replace_board(Board,X1,Y1,X2,Y2,0,NBoard) :- replace_element(Board,X1,Y1,[' ','B','C'],TBoard), replace_element(TBoard,X2,Y2,[' ','P','Q'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,1,NBoard) :- replace_element(Board,X1,Y1,[' ','P','C'],TBoard), replace_element(TBoard,X2,Y2,[' ','B','Q'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,2,NBoard) :- replace_element(Board,X1,Y1,[' ','B','Q'],TBoard), replace_element(TBoard,X2,Y2,[' ','P','C'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,3,NBoard) :- replace_element(Board,X1,Y1,[' ','P','Q'],TBoard), replace_element(TBoard,X2,Y2,[' ','B','C'],NBoard).

% Jogador 1 coloca as torres
pick_tower(Board) :- display_board(Board), write('Player 1: State the vertical coordinate of the first white tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the first white tower: (Ex: 1.)'), read(N), write('\n'), char_code(Character,Charcode), Y is Charcode-97, X is N-1, insert_tower(Board, X, Y, 'L', NBoard), pick_tower2(NBoard).
pick_tower2(Board) :- display_board(Board), write('Player 1: State the vertical coordinate of the second white tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second white tower: (Ex: 1.)'), read(N), write('\n'), char_code(Character,Charcode), Y is Charcode-97, X is N-1, insert_tower(Board, X, Y, 'L', NBoard), pick_tower3(NBoard).
pick_tower3(Board) :- display_board(Board), write('Player 1: State the vertical coordinate of the first black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the first black tower: (Ex: 1.)'), read(N), write('\n'), char_code(Character,Charcode), Y is Charcode-97, X is N-1, insert_tower(Board, X, Y, 'T', NBoard), pick_tower4(NBoard).
pick_tower4(Board) :- display_board(Board), write('Player 1: State the vertical coordinate of the second black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second black tower: (Ex: 1.)'), read(N), write('\n'), char_code(Character,Charcode), Y is Charcode-97, X is N-1, insert_tower(Board, X, Y, 'T', NBoard), pick_colour(NBoard).

% Jogador 2 escolhe a cor
pick_colour(Board) :- display_board(Board), write('Player 2: Choose your colour. From now on you will be identified with your colour (white/black): '), read(Colour), colour_picked(Board,Colour).

% Play time!
colour_picked(Board,'white') :- write('White: Your turn to play\n'), display_board(Board). 
colour_picked(Board,'w') :- write('White: Your turn to play\n'), display_board(Board). 
colour_picked(Board,'black') :- write('White: Your turn to play\n'), display_board(Board). 
colour_picked(Board,'b') :- write('White: Your turn to play\n'), display_board(Board). 

%replace_element(L,X,Y,NElem,NL)
valid_slide(Board,X,Y,NX,NY) :- return_element(Board,X,Y,Elem), valid_slide_aux(Board,X,Y,NX,NY,Elem).
valid_slide_aux(Board,X,Y,X,Y,Elem) :- return_element(Board,X,Y,Elem). % Mera verificacao, muito provavelmente ate se pode apagar.
valid_slide_aux(Board,X,Y,NX,NY,Elem) :- X >= 0, Y >= 0, board_size(Board, SizeX, SizeY), X < SizeX, Y < SizeY,
										 return_element(Board,X,Y,Elem). 
										 A is X+1, B is Y+1, C is X-1, D is Y+1,
										 valid_slide_aux(Board,A,Y,NX,NY,Elem),
										 valid_slide_aux(Board,X,B,NX,NY,Elem),
										 valid_slide_aux(Board,C,Y,NX,NY,Elem),
										 valid_slide_aux(Board,X,D,NX,NY, Elem).
slide_tile(Board,X,Y,NX,NY,NBoard) :- return_element(Board,X,Y,Elem), replace_element(Board,NX,NY,Elem,TBoard), replace_element(TBoard,X,Y,[' ', ' ', ' '],NBoard).
remove_tile(Board,X,Y,NBoard) :- replace_element(Board,X,Y,[' ', ' ', ' '],NBoard).
move_tower(Board,X,Y,NX,NY,NBoard) :- return_element(Board,X,Y,[Tower|_]), insert_tower(Board, NX, NY, Tower,TBoard), remove_tower(TBoard,X,Y,NBoard).
pass(Board).