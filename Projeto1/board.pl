%MODULES
:- use_module(library(random)).
:- dynamic board_cell/3.
:- dynamic board_length/1.
:- dynamic sink_streak/2.
:- dynamic current_player/1.
:- dynamic number_squares/1.
:- dynamic number_circles/1.
:- dynamic number_blacks/1.
:- dynamic number_whites/1.

%Database manipulation
purge_database(N) :- N > 0, purge_database_aux(0,0), retract(board_length(N)), retract(sink_streak(_,_)), retract(current_player(_)), retract(number_circles(_)), retract(number_squares(_)), retract(number_blacks(_)), retract(number_whites(_)).
purge_database_aux(Row, Col) :- board_length(Length), Row < Length, Col < Length, !, retract(board_cell(Row, Col, _)), NCol is Col + 1, purge_database_aux(Row,NCol).
purge_database_aux(Row, _) :- board_length(Length), Row < Length, !, NRow is Row + 1, purge_database_aux(NRow, 0).
purge_database_aux(Row, _) :- board_length(Row).

create_database(N) :- N > 0, assert(number_squares(0)), assert(number_circles(0)), assert(number_blacks(0)), assert(number_whites(0)), assert(sink_streak('white', 0)), assert(current_player('white')), assert(board_length(N)), create_database_aux(0, 0).
create_database_aux(Row, Col) :- board_length(Length), Row < Length, Col < Length, !, assert(board_cell(Row, Col, [' ', ' ', ' '])), NCol is Col + 1, create_database_aux(Row,NCol).
create_database_aux(Row, _) :- board_length(Length), Row < Length, !, NRow is Row + 1, create_database_aux(NRow, 0).
create_database_aux(Row, _) :- board_length(Row).

%Board display
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
start_game :- board_length(Length), purge_database(Length), game. %Esta linha vai ser apagada, so esta aqui para nao ter de fazer isto a toda a hora.
start_game :- game.
game :- write('Please state the board you want (major/minor): '), read(X), create_board(X), pick_tower, pick_colour, game_cycle, write('\nPlayer '), current_player(Player), write(Player), write(' has won the game!').
create_board(minor) :- create_database(5), randomize_board_minor.
create_board(major) :- create_database(7), randomize_board_major.

%Randomize board
randomize(N) :- random(0,4,N).

/****************

Database modifiers

*****************/

%Changes a tile's content
change_tile(X,Y,[Tower,Colour,Shape]) :- retract(board_cell(X,Y,_)), assert(board_cell(X,Y,[Tower,Colour,Shape])).

%Adds a colour/shape to the colour/shape counter
add_colour_shape(Colour, Shape) :- add_colour(Colour), add_shape(Shape).
add_colour('B') :- number_whites(N), NW is N+1, retract(number_whites(N)), assert(number_whites(NW)).
add_colour('P') :- number_blacks(N), NB is N+1, retract(number_blacks(N)), assert(number_blacks(NB)).
add_shape('Q') :- number_squares(N), NS is N+1, retract(number_squares(N)), assert(number_squares(NS)).
add_shape('C') :- number_circles(N), NC is N+1, retract(number_circles(N)), assert(number_circles(NC)). 

%Removes a colour/shape to the colour/shape counter
remove_colour_shape(Colour, Shape) :- remove_colour(Colour), remove_shape(Shape).
remove_colour('B') :- number_whites(N), NW is N-1, retract(number_whites(N)), assert(number_whites(NW)).
remove_colour('P') :- number_blacks(N), NB is N-1, retract(number_blacks(N)), assert(number_blacks(NB)).
remove_shape('Q') :- number_squares(N), NS is N-1, retract(number_squares(N)), assert(number_squares(NS)).
remove_shape('C') :- number_circles(N), NC is N-1, retract(number_circles(N)), assert(number_circles(NC)). 

%Tiles sinked counter - used to check win condition
sink_count(Player) :- sink_streak(Player,Streak), NStreak is Streak+1, retract(sink_streak(_, Streak)), assert(sink_streak(Player,NStreak)).
sink_count(Player) :- sink_streak(OPlayer,_), Player \= OPlayer, retract(sink_streak(_,_)), assert(sink_streak(Player, 1)).

%Change current Player
change_player :- retract(current_player('white')), assert(current_player('black')). 
change_player :- retract(current_player('black')), assert(current_player('white')). 

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
randomize_board_major_35 :- randomize(N), replace_board(0,4,6,2,N).	

randomize_board_minor :- randomize(N), replace_board(2,1,2,3,N), randomize_board_minor_3.
randomize_board_minor_3 :- randomize(N), replace_board(2,0,2,4,N), randomize_board_minor_5.
randomize_board_minor_5 :- randomize(N), replace_board(1,0,3,4,N), randomize_board_minor_7.
randomize_board_minor_7 :- randomize(N), replace_board(1,1,3,3,N), randomize_board_minor_9.
randomize_board_minor_9 :- randomize(N), replace_board(1,2,3,2,N), randomize_board_minor_11.
randomize_board_minor_11 :- randomize(N), replace_board(1,3,3,1,N), randomize_board_minor_13.
randomize_board_minor_13 :- randomize(N), replace_board(0,3,4,1,N), randomize_board_minor_15.
randomize_board_minor_15 :- randomize(N), replace_board(0,2,4,2,N).

% Auxiliary function of the randomizer. 0-BC 1-PC 2-BQ 3-PQ
replace_board(X1,Y1,X2,Y2,0) :- change_tile(X1,Y1,[' ','B','C']), change_tile(X2,Y2,[' ','P','Q']), add_colour_shape('B', 'C'), add_colour_shape('P', 'Q').
replace_board(X1,Y1,X2,Y2,1) :- change_tile(X1,Y1,[' ','P','C']), change_tile(X2,Y2,[' ','B','Q']), add_colour_shape('P', 'C'), add_colour_shape('B', 'Q').
replace_board(X1,Y1,X2,Y2,2) :- change_tile(X1,Y1,[' ','B','Q']), change_tile(X2,Y2,[' ','P','C']), add_colour_shape('B', 'Q'), add_colour_shape('P', 'C').
replace_board(X1,Y1,X2,Y2,3) :- change_tile(X1,Y1,[' ','P','Q']), change_tile(X2,Y2,[' ','B','C']), add_colour_shape('P', 'Q'), add_colour_shape('B', 'C').

% Player 1 picks the towers
pick_tower_aux(Character, Number, Tower) :- Tower == 'L', char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, insert_tower(X, Y, Tower).
pick_tower_aux(Character, Number, Tower) :- Tower == 'T', char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, insert_tower(X, Y, Tower).
pick_tower :- display_board, write('Player 1: State the vertical coordinate of the first white tower: (Ex: a.)'), read(Character), write('State the horizontal coordinate of the first white tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'L'), pick_tower2.
pick_tower2 :- display_board, write('Player 1: State the vertical coordinate of the second white tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second white tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'L'), pick_tower3.
pick_tower3 :- display_board, write('Player 1: State the vertical coordinate of the first black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the first black tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'T'), pick_tower4.
pick_tower4 :- display_board, write('Player 1: State the vertical coordinate of the second black tower: (Ex: a.)'), read(Character),  write('State the horizontal coordinate of the second black tower: (Ex: 1.)'), read(Number), pick_tower_aux(Character, Number, 'T').

% Player two picks the colour
pick_colour :- display_board, write('Player 2: Choose your colour. From now on you will be identified with your colour (white/black): '), read(Colour), colour_picked(Colour).

% Game cycle

game_cycle :- repeat, make_play, change_player, (check_winning_condition).

% Play time!
colour_picked('white') :- write('White: Your turn to play\n'), display_board. 
colour_picked('w') :- write('White: Your turn to play\n'), display_board. 
colour_picked('black') :- write('White: Your turn to play\n'), display_board. 
colour_picked('b') :- write('White: Your turn to play\n'), display_board. 

% Make play
make_play :- display_board, current_player(Player), write(Player), write(': Your turn to play'), nl, write('Make your move (slide/sink/movetower/pass): '), read(Move), make_play_aux(Move).
make_play_aux(Move) :- Move == 'sink', sink_tile_aux.
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

sink_tile_aux :-	write('\nState the vertical coordinate of the tile you want to remove: (Ex: a.)'), read(Character),
					write('\nState the horizontal coordinate of the tile you want to remove: (Ex: 1.)'), read(Number),
					char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1,
					sink_tile(X,Y).

move_tower_aux :- 	write('\nState the vertical coordinate of the tower you want to move: (Ex: a.)'), read(Character),
					write('\nState the horizontal coordinate of the tower you want to move: (Ex: 1.)'), read(Number),
					write('\nState the vertical coordinate of the tower you want to move the tower to: (Ex: a.)'), read(NCharacter),
					write('\nState the horizontal coordinate of the tower you want to move the tile to: (Ex: 1.)'), read(NNumber),
					char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, % <--- TODO: Fazer desta linha uma funcao e usar na pick_tower e nas 3 jogadas
					char_code(NCharacter,NCharcode), write('\n'), NY is NCharcode-97, NX is NNumber-1,
					move_tower(X,Y,NX,NY).

% The four possible plays. TODO: Make invalid moves impossible
slide_tile(X,Y,NX,NY) :- board_cell(X,Y,Elem), change_tile(NX,NY,Elem), change_tile(X,Y,[' ', ' ', ' ']).
sink_tile(X,Y) :- board_cell(X, Y, [' ',C,S]), !, remove_colour_shape(C,S), change_tile(X,Y,[' ', ' ', ' ']).
move_tower(X,Y,NX,NY) :- board_cell(X,Y,[Tower|_]), insert_tower(NX, NY, Tower), remove_tower(X,Y).
pass.

% Check end game condition
check_winning_condition :- sink_streak(_, 4).


valid_slide(X, Y, FinalX, FinalY) :- slidable_tiles(X,Y,Tiles), member([FinalX,FinalY], Tiles).

connected_board :- 	board_cell(X, Y, [_, 'P', _]), reachable_tiles(X, Y, Tiles), length(Tiles, L), number_blacks(B), number_whites(W), !, L is B + W.

reachable_tiles(X, Y, Tiles) :- reachable_tiles_aux([[X,Y]],[], Tiles).
reachable_tiles_aux([],_, []).
reachable_tiles_aux([Next|T], Visited, Reachable) :- member(Next, Visited), !, reachable_tiles_aux(T, Visited, Reachable).
reachable_tiles_aux([[X, Y]|T], Visited, [[X, Y]|Reachable]) :- board_cell(X, Y, Cell), Cell \= [' ', ' ', ' '], !,
	neighbour_tiles(X,Y,Neighbours), append(T, Neighbours, NT),
	reachable_tiles_aux(NT, [[X,Y]|Visited], Reachable).
reachable_tiles_aux([Next|T], Visited, Reachable) :- reachable_tiles_aux(T, [Next|Visited], Reachable).

%neighbour positions
neighbour_tiles(X, Y, [Alt1, Alt2, Alt3, Alt4]) :- 
	NX is X + 1, PX is X - 1, NY is Y + 1, PY is Y - 1,
	Alt1 = [NX, Y], Alt2 = [PX, Y], Alt3 = [X, NY], Alt4 = [X, PY].

%searching islands
dark_island(X, Y, Island) :- board_cell(X,Y,[_,'P',_]), dark_island_search([[X,Y]], [], Island).
dark_island_search([], _, []).
dark_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, dark_island_search(T,Visited,Island).
dark_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,'P',_]), !,
	neighbour_tiles(X,Y,Neighbours), append(T, Neighbours, NT),
	dark_island_search(NT, [[X,Y]|Visited], Island).
dark_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,'P',_]), !,
	dark_island_search(T,[[X,Y]|Visited], Island).
	
light_island(X, Y, Island) :- board_cell(X,Y,[_,'B',_]), light_island_search([[X,Y]], [], Island).
light_island_search([], _, []).
light_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, light_island_search(T,Visited,Island).
light_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,'B',_]), !,
	neighbour_tiles(X,Y,Neighbours), append(T, Neighbours, NT),
	light_island_search(NT, [[X,Y]|Visited], Island).
light_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,'B',_]), !,
	light_island_search(T,[[X,Y]|Visited], Island).

circle_island(X, Y, Island) :- board_cell(X,Y,[_,_,'C']), circle_island_search([[X,Y]], [], Island).
circle_island_search([], _, []).
circle_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, circle_island_search(T,Visited,Island).
circle_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,_,'C']), !,
	neighbour_tiles(X,Y,Neighbours), append(T, Neighbours, NT),
	circle_island_search(NT, [[X,Y]|Visited], Island).
circle_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,_,'C']), !,
	circle_island_search(T,[[X,Y]|Visited], Island).
	
square_island(X, Y, Island) :- board_cell(X,Y,[_,_,'Q']), square_island_search([[X,Y]], [], Island).
square_island_search([], _, []).
square_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, square_island_search(T,Visited,Island).
square_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,_,'Q']), !,
	neighbour_tiles(X,Y,Neighbours), append(T, Neighbours, NT),
	square_island_search(NT, [[X,Y]|Visited], Island).
square_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,_,'Q']), !,
	square_island_search(T,[[X,Y]|Visited], Island).
	
%searching slidable positions
slidable_tiles(X, Y, Tiles) :- \+ board_cell(X, Y, [' ',_,_]),
	neighbour_tiles(X,Y,Neighbours), slidable_tiles_search(Neighbours, [[X,Y]], PTiles),
	slidable_tiles_valid(X, Y, PTiles, Tiles).

slidable_tiles_search([],_,[]).
slidable_tiles_search([Tile|T], Visited, PTiles) :- member(Tile, Visited), !, slidable_tiles_search(T, Visited, PTiles).
slidable_tiles_search([[X,Y]|T], Visited, [[X,Y]|PTiles]) :- \+ member([X,Y], Visited), board_cell(X, Y, [' ',' ',' ']), !,
	neighbour_tiles(X, Y, Neighbours), append(T, Neighbours, NT),
	slidable_tiles_search(NT, [[X,Y]|Visited], PTiles).
slidable_tiles_search([[X,Y]|T], Visited, PTiles) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[' ',' ',' ']), !,
	slidable_tiles_search(T,[[X,Y]|Visited], PTiles).

slidable_tiles_valid(_,_,[], []).
slidable_tiles_valid(StartX, StartY, [[X,Y]|PTiles],Tiles) :- slidable_tiles_valid(StartX, StartY, PTiles, NTiles),
	board_cell(StartX,StartY,Cell), change_tile(StartX, StartY, [' ',' ',' ']), change_tile(X,Y,Cell),
	(connected_board -> append([[X,Y]],NTiles, Tiles); Tiles = NTiles), change_tile(StartX, StartY, Cell), change_tile(X, Y, [' ',' ',' ']). 
