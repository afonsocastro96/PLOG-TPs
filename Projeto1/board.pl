%MODULES
:- use_module(library(random)).
:- dynamic board_cell/3.
:- dynamic board_length/1.

purge_database(N) :- N > 0, purge_database_aux(0,0), retract(board_length(N)).
purge_database_aux(Row, Col) :- board_length(Length), Row < Length, Col < Length, !, retract(board_cell(Row, Col, _)), NCol is Col + 1, purge_database_aux(Row,NCol).
purge_database_aux(Row, _) :- board_length(Length), Row < Length, !, NRow is Row + 1, purge_database_aux(NRow, 0).
purge_database_aux(Row, _) :- board_length(Row).

create_database(N) :- N > 0, assert(board_length(N)), create_database_aux(0, 0).
create_database_aux(Row, Col) :- board_length(Length), Row < Length, Col < Length, !, assert(board_cell(Row, Col, [' ', ' ', ' '])), NCol is Col + 1, create_database_aux(Row,NCol).
create_database_aux(Row, _) :- board_length(Length), Row < Length, !, NRow is Row + 1, create_database_aux(NRow, 0).
create_database_aux(Row, _) :- board_length(Row).

display_board :- write_col_coords, display_board_row(0).
display_board_row(Row) :- board_length(Length), Row < Length, !, write_border, write_line(Row), NRow is Row + 1, display_board_row(NRow).
display_board_row(Row) :- board_length(Row), !, write_border.

write_border :- write('   '), write_border_aux(0).
write_border_aux(Col) :- board_length(Col), !, write('+\n').
write_border_aux(Col) :- board_length(Length), Col < Length, !, write('+---'), NCol is Col + 1, write_border_aux(NCol).

write_aux_line(_, Col) :- board_length(Col), !, write('|\n').
write_aux_line(Row, Col) :- board_length(Length), Col < Length, !, write('|'), write_elem(Row, Col), NCol is Col + 1, write_aux_line(Row, NCol).

write_line(Row) :- Number is Row + 1, format(' ~d ', [Number]), write_aux_line(Row, 0).

write_elem(Row,Col) :- board_cell(Row, Col, [Tower, Colour, Shape]), write(Tower), write(Colour), write(Shape).

write_col_coords :- write('   '), write_col_coords_aux(0).
write_col_coords_aux(Col) :- board_length(Length), Col < Length, !, Charcode is Col + 65, format('  ~c ', [Charcode]), NCol is Col + 1, write_col_coords_aux(NCol).
write_col_coords_aux(Col) :- board_length(Length), Col is Length, !, write(' \n').

%Insert and remove a tower in a given place
insert_tower(X,Y,'L') :- board_cell(X,Y,[' ',_,_]), \+ board_cell(X,Y,[_,'P','Q']), \+ board_cell(X,Y,[_, ' ', ' ']), board_cell(X,Y,[_,Colour,Shape]), change_tile(X,Y,['L',Colour,Shape]).
insert_tower(X,Y,'T') :- board_cell(X,Y,[' ',_,_]), \+ board_cell(X,Y,[_,'B','C']), \+ board_cell(X,Y,[_, ' ', ' ']), board_cell(X,Y,[_,Colour,Shape]), change_tile(X,Y,['T',Colour,Shape]).
remove_tower(X,Y) :- board_cell(X,Y,[_,Colour,Shape]), change_tile(X,Y,[' ', Colour, Shape]).

%Start game
start_game :- write('Please state the board you want (major/minor): '), read(X), create_board(X).
start_game :- board_length(Length), purge_database(Length), write('Please state the board you want (major/minor): '), read(X), create_board(X).
create_board(minor) :- create_database(5), randomize_board_minor.
create_board(major) :- create_database(7), randomize_board_major.

%Randomize board
%NOTA: ISTO NAO ESTA A GERAR A COMBINACAO PQ (numero 3).
randomize(N) :- random(0,3,N).

%Changes a tile's content
change_tile(X,Y,[Tower,Colour,Shape]) :- retract(board_cell(X,Y,_)), assert(board_cell(X,Y,[Tower,Colour,Shape])).

% Board randomizer
randomize_board_major :- randomize(N), replace_board(3,2,3,4,N), randomize_board_major_3.
randomize_board_major_3 :- randomize(N), replace_board(3,1,3,5,N), randomize_board_major_5.
randomize_board_major_5 :- randomize(N), replace_board(3,0,3,6,N), randomize_board_major_7.
randomize_board_major_7 :- randomize(N), replace_board(2,0,4,6,N), randomize_board_major_9.
randomize_board_major_9 :- randomize(N), replace_board(2,1,4,5,N), randomize_board_major_11.
randomize_board_major_11 :- randomize(N), replace_board(2,2,4,4,N), randomize_board_major_13.
randomize_board_major_13 :- randomize(N), replace_board(2,3,4,3,N), randomize_board_major_15.
randomize_board_major_15 :- randomize(N), replace_board(2,4,4,2,N), randomize_board_major_17.
randomize_board_major_17 :- randomize(N), replace_board(2,5,4,1,N), randomize_board_major_19.
randomize_board_major_19 :- randomize(N), replace_board(2,6,4,0,N), randomize_board_major_21.
randomize_board_major_21 :- randomize(N), replace_board(1,5,5,1,N), randomize_board_major_23.
randomize_board_major_23 :- randomize(N), replace_board(1,4,5,2,N), randomize_board_major_25.
randomize_board_major_25 :- randomize(N), replace_board(1,3,5,3,N), randomize_board_major_27.
randomize_board_major_27 :- randomize(N), replace_board(1,2,5,4,N), randomize_board_major_29.
randomize_board_major_29 :- randomize(N), replace_board(1,1,5,5,N), randomize_board_major_31.
randomize_board_major_31 :- randomize(N), replace_board(0,2,6,4,N), randomize_board_major_33.
randomize_board_major_33 :- randomize(N), replace_board(0,3,6,3,N), randomize_board_major_35.
randomize_board_major_35 :- randomize(N), replace_board(0,4,6,2,N), pick_tower.

randomize_board_minor :- randomize(N), replace_board(2,1,2,3,N), randomize_board_minor_3.
randomize_board_minor_3 :- randomize(N), replace_board(2,0,2,4,N), randomize_board_minor_5.
randomize_board_minor_5 :- randomize(N), replace_board(1,0,3,4,N), randomize_board_minor_7.
randomize_board_minor_7 :- randomize(N), replace_board(1,1,3,3,N), randomize_board_minor_9.
randomize_board_minor_9 :- randomize(N), replace_board(1,2,3,2,N), randomize_board_minor_11.
randomize_board_minor_11 :- randomize(N), replace_board(1,3,3,1,N), randomize_board_minor_13.
randomize_board_minor_13 :- randomize(N), replace_board(0,3,4,1,N), randomize_board_minor_15.
randomize_board_minor_15 :- randomize(N), replace_board(0,2,4,2,N), pick_tower.

% Auxiliary function of the randomizer. 0-BC 1-PC 2-BQ 3-PQ
replace_board(X1,Y1,X2,Y2,0) :- change_tile(X1,Y1,[' ','B','C']), change_tile(X2,Y2,[' ','B','C']).
replace_board(X1,Y1,X2,Y2,1) :- change_tile(X1,Y1,[' ','P','C']), change_tile(X2,Y2,[' ','B','Q']).
replace_board(X1,Y1,X2,Y2,2) :- change_tile(X1,Y1,[' ','B','Q']), change_tile(X2,Y2,[' ','P','C']).
replace_board(X1,Y1,X2,Y2,3) :- change_tile(X1,Y1,[' ','P','Q']), change_tile(X2,Y2,[' ','B','C']).

% Player 1 picks the towers
pick_tower_aux(Character, Number, Tower) :- Tower == 'L', char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, insert_tower(X, Y, Tower).
pick_tower_aux(Character, Number, Tower) :- Tower == 'T', char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, insert_tower(X, Y, Tower).
pick_tower :- display_board, write('Player 1: State the vertical coordinate of the first white tower: (Ex: a.)'), read(Character), write('State the horizontal coordinate of the first white tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'L'), pick_tower2.
pick_tower2 :- display_board, write('Player 1: State the vertical coordinate of the second white tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second white tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'L'), pick_tower3.
pick_tower3 :- display_board, write('Player 1: State the vertical coordinate of the first black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the first black tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'T'), pick_tower4.
pick_tower4 :- display_board, write('Player 1: State the vertical coordinate of the second black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second black tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'T'), pick_colour.

% Player two picks the colour
pick_colour :- display_board, write('Player 2: Choose your colour. From now on you will be identified with your colour (white/black): '), read(Colour), colour_picked(Colour), make_play.

% Play time!
colour_picked('white') :- write('White: Your turn to play\n'), display_board. 
colour_picked('w') :- write('White: Your turn to play\n'), display_board. 
colour_picked('black') :- write('White: Your turn to play\n'), display_board. 
colour_picked('b') :- write('White: Your turn to play\n'), display_board. 

% Make play

make_play :- write('Make your move (slide/remove/movetower/pass): '), read(Move), make_play_aux(Move), display_board.
make_play_aux(Move) :- Move == 'remove', remove_tile_aux.
make_play_aux(Move) :- Move == 'movetower', move_tower_aux.
make_play_aux(Move) :- Move == 'slide', slide_tile_aux.
make_play_aux(Move) :- Move == 'pass', pass.

% Treat each play individually

slide_tile_aux :- 	write('\nState the vertical coordinate of the tile you want to slide: (Ex: a.)'), read(Character),
					write('\nState the horizontal coordinate of the tile you want to slide: (Ex: 1.)'), read(Number),
					write('\nState the vertical coordinate of the tile you want to put the tile in: (Ex: a.)'), read(NCharacter),
					write('\nState the horizontal coordinate of the tile you want to put the tile in: (Ex: 1.)'), read(NNumber),
					char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, % <--- TODO: Fazer desta linha uma funcao e usar na pick_tower e nas 3 jogadas
					char_code(NCharacter,NCharcode), write('\n'), NY is NCharcode-97, NX is NNumber-1,
					slide_tile(X,Y,NX,NY).

remove_tile_aux :-	write('\nState the vertical coordinate of the tile you want to remove: (Ex: a.)'), read(Character),
					write('\nState the horizontal coordinate of the tile you want to remove: (Ex: 1.)'), read(Number),
					char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1,
					remove_tile(X,Y).

move_tower_aux :- 	write('\nState the vertical coordinate of the tower you want to move: (Ex: a.)'), read(Character),
					write('\nState the horizontal coordinate of the tower you want to move: (Ex: 1.)'), read(Number),
					write('\nState the vertical coordinate of the tower you want to move the tower to: (Ex: a.)'), read(NCharacter),
					write('\nState the horizontal coordinate of the tower you want to move the tile to: (Ex: 1.)'), read(NNumber),
					char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, % <--- TODO: Fazer desta linha uma funcao e usar na pick_tower e nas 3 jogadas
					char_code(NCharacter,NCharcode), write('\n'), NY is NCharcode-97, NX is NNumber-1,
					move_tower(X,Y,NX,NY).

% The four possible plays. TODO: Make invalid moves impossible
slide_tile(X,Y,NX,NY) :- board_cell(X,Y,Elem), change_tile(NX,NY,Elem), change_tile(X,Y,[' ', ' ', ' ']).
remove_tile(X,Y) :- change_tile(X,Y,[' ', ' ', ' ']).
move_tower(X,Y,NX,NY) :- board_cell(X,Y,[Tower|_]), insert_tower(NX, NY, Tower), remove_tower(X,Y).
pass.

%valid_slide
%valid_slide(Board,X,Y,NX,NY,Visited) :- X >= 0, Y >= 0, board_size(Board, SizeX, SizeY), X < SizeX, Y < SizeY, \+ member(Element, Visited),
%										 return_element(Board,X,Y,[' ',' ',' ']), valid_slide_aux(Board,X,Y,NX,NY).
%valid_slide_aux(Board,X,Y,X,Y).
%valid_slide_aux(Board,X,Y,NX,NY) :- A is X+1, valid_slide(Board,A,Y,NX,NY). 
%valid_slide_aux(Board,X,Y,NX,NY) :- B is Y+1, valid_slide(Board,X,B,NX,NY).
%valid_slide_aux(Board,X,Y,NX,NY) :- C is X-1, valid_slide(Board,C,Y,NX,NY).
%valid_slide_aux(Board,X,Y,NX,NY) :- D is Y-1, valid_slide(Board,X,D,NX,NY).