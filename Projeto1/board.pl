%MODULES

use_module(library(random)).

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
	[[[' ',' ',' '], [' ',' ',' '], ['L','B','C'], [' ','B','Q'],
	 [' ',' ',' ']],
	[[' ','P','Q'], [' ','P','C'], [' ','P','C'], ['T','B','Q'],
	 [' ',' ',' ']],
 	[[' ','P','Q'], [' ','P','Q'], [' ',' ',' '], [' ','P','C'],
 	 [' ','P','Q']],
 	[[' ',' ',' '], ['T','B','Q'], [' ','P','Q'], [' ','B','C'],
 	 [' ','B','Q']],
 	[[' ',' ',' '], [' ',' ',' '], [' ','P','C'],
 	 [' ','P','C'], [' ',' ',' ']]].

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

write_col_coords([Line | Board]) :- write('   '),
write_col_coords_aux(Line,65).

write_col_coords_aux([Elem],Charcode) :- char_code(Character,Charcode),
write('  '), write(Character), write('\n').

write_col_coords_aux([Elem|Line],Charcode) :- Line \= [],
char_code(Character,Charcode), write('  '), write(Character),
write(' '), Nextchar is Charcode+1, write_col_coords_aux(Line,Nextchar). 


%Replace L[X][Y] with NElem
replace_element(L,X,Y,NElem,NL) :- replace_element_aux_x(L,X,Y,NElem,NL).
replace_element_aux_x([H|T],0,Y,NElem,[NH|T]) :- replace_element_aux_y(H,Y,NElem, NH).
replace_element_aux_x([H|L],X,Y,NElem,[H|NL]) :- X>0,Z is X-1, replace_element_aux_x(L,Z,Y,NElem,NL).
replace_element_aux_y([H|T],0,NElem,[NElem|T]).
replace_element_aux_y([H|L],Y,NElem,[H|NL]) :- Y>0,Z is Y-1, replace_element_aux_y(L,Z,NElem,NL).	

% Start game

start_game :- write('Please state the board you want (major/minor): '), read(X), create_board(Board,X).
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
randomize_board_major_35(Board) :- randomize(N), replace_board(Board,0,4,6,2,N, NBoard), write('cenas\n'), display_board(NBoard).

randomize_board_minor(Board) :- randomize(N), replace_board(Board,2,1,2,3,N, NBoard), randomize_board_minor_3(NBoard).
randomize_board_minor_3(Board) :- randomize(N), replace_board(Board,2,0,2,4,N, NBoard), randomize_board_minor_5(NBoard).
randomize_board_minor_5(Board) :- randomize(N), replace_board(Board,1,0,3,4,N, NBoard), randomize_board_minor_7(NBoard).
randomize_board_minor_7(Board) :- randomize(N), replace_board(Board,1,1,3,3,N, NBoard), randomize_board_minor_9(NBoard).
randomize_board_minor_9(Board) :- randomize(N), replace_board(Board,1,2,3,2,N, NBoard), randomize_board_minor_11(NBoard).
randomize_board_minor_11(Board) :- randomize(N), replace_board(Board,1,3,3,1,N, NBoard), randomize_board_minor_13(NBoard).
randomize_board_minor_13(Board) :- randomize(N), replace_board(Board,0,3,4,1,N, NBoard), randomize_board_minor_15(NBoard).
randomize_board_minor_15(Board) :- randomize(N), replace_board(Board,0,2,4,2,N, NBoard), write('cenas\n'), display_board(NBoard).


% 0-BC 1-PC 2-BQ 3-PQ
replace_board(Board,X1,Y1,X2,Y2,0,NBoard) :- replace_element(Board,X1,Y1,[' ','B','C'],TBoard), replace_element(TBoard,X2,Y2,[' ','P','Q'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,1,NBoard) :- replace_element(Board,X1,Y1,[' ','P','C'],TBoard), replace_element(TBoard,X2,Y2,[' ','B','Q'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,2,NBoard) :- replace_element(Board,X1,Y1,[' ','B','Q'],TBoard), replace_element(TBoard,X2,Y2,[' ','P','C'],NBoard).
replace_board(Board,X1,Y1,X2,Y2,3,NBoard) :- replace_element(Board,X1,Y1,[' ','P','Q'],TBoard), replace_element(TBoard,X2,Y2,[' ','B','C'],NBoard).