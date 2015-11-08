%MODULES
:- use_module(library(random)).
:- use_module(library(lists)).
:- dynamic board_cell/3.
:- dynamic board_length/1.
:- dynamic sink_streak/2.
:- dynamic current_player/1.
:- dynamic number_squares/1.
:- dynamic number_circles/1.
:- dynamic number_blacks/1.
:- dynamic number_whites/1.
:- dynamic number_pass/2.
:- dynamic bot_colour/1.

%Database manipulation
purge_database(N) :-	N > 0, purge_database_aux(0,0), retract(board_length(N)), retract(sink_streak(_,_)), retract(current_player(_)),
retract(number_circles(_)), retract(number_squares(_)), retract(number_blacks(_)), retract(number_whites(_)),
retract(number_pass('white',_)), retract(number_pass('black',_)), (bot_colour(_) -> retract(bot_colour(_)); true).
purge_database_aux(Row, Col) :- board_length(Length), Row < Length, Col < Length, !, retract(board_cell(Row, Col, _)), NCol is Col + 1, purge_database_aux(Row,NCol).
purge_database_aux(Row, _) :- board_length(Length), Row < Length, !, NRow is Row + 1, purge_database_aux(NRow, 0).
purge_database_aux(Row, _) :- board_length(Row).

create_database(N) :- 	N > 0, assert(number_squares(0)), assert(number_circles(0)), assert(number_blacks(0)), assert(number_whites(0)),
assert(sink_streak('white', 0)), assert(current_player('white')), assert(number_pass('white', 0)), assert(number_pass('black', 0)),
assert(board_length(N)), create_database_aux(0, 0).
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
write_col_coords_aux(Col) :- board_length(Length), Col is Length, !, nl.

%Insert and remove a tower in a given place
insert_tower(X,Y,'L') :- board_cell(X,Y,[' ','B',Shape]), !, change_tile(X,Y,['L','B',Shape]).
insert_tower(X,Y,'L') :- board_cell(X,Y,[' ',Colour,'C']), !, change_tile(X,Y,['L',Colour,'C']).
insert_tower(X,Y,'T') :- board_cell(X,Y,[' ','P',Shape]), !, change_tile(X,Y,['T','P',Shape]).
insert_tower(X,Y,'T') :- board_cell(X,Y,[' ',Colour,'Q']), !, change_tile(X,Y,['T',Colour,'Q']).
remove_tower(X,Y) :- board_cell(X,Y,[Tower,Colour,Shape]), Tower \= ' ', change_tile(X,Y,[' ', Colour, Shape]).

%Start game
start_game :- pick_play_mode.
pick_play_mode :- write('Please state the desired play mode (1- Player vs Computer 2- Player vs Player 3- Computer vs Computer): '), read(Mode), test_mode(Mode).
test_mode(1) :- game_cvp.
test_mode(2) :- game_pvp.
test_mode(3) :- game_cvc.
test_mode(_) :- nl, write('Invalid mode!'), nl, pick_play_mode.
game_pvp :- once(ask_board), once(pick_towers), !, pick_colour, game_cycle(Winner), end_game(Winner).
game_cvp :- once(ask_board), once(pick_towers), !, bot_pick_colour(Colour), assert(bot_colour(Colour)), nl, write('Bot picked the '), write(Colour), write(' colour'), nl, game_cycle_cvp(Winner), end_game(Winner).
game_cvc :- once(ask_board), once(randomize_towers), !, game_cycle_cvc(Winner), end_game(Winner). 

%Creates the chosen board.
create_board(minor) :- create_database(5), randomize_board_minor.
create_board(major) :- create_database(7), randomize_board_major.
create_board(_) :- write('Invalid type of board!'), nl, ask_board.

/****************

Database modifiers

*****************/

%Changes a tile's content
change_tile(X,Y,[Tower,Colour,Shape]) :-
	retract(board_cell(X,Y,[_, PColour, PShape])), remove_colour_shape(PColour, PShape),
	assert(board_cell(X,Y,[Tower,Colour,Shape])), add_colour_shape(Colour, Shape).
add_tile(X,Y,[Tower,Colour, Shape]) :-
	board_cell(X,Y,[' ',' ',' ']), change_tile(X, Y, [Tower,Colour,Shape]).
remove_tile(X, Y) :-
	change_tile(X,Y,[' ',' ',' ']).

%Adds a colour/shape to the colour/shape counter
add_colour_shape(Colour, Shape) :- add_colour(Colour), add_shape(Shape).
add_colour('B') :- number_whites(N), NW is N+1, retract(number_whites(N)), assert(number_whites(NW)).
add_colour('P') :- number_blacks(N), NB is N+1, retract(number_blacks(N)), assert(number_blacks(NB)).
add_colour(' ').
add_shape('Q') :- number_squares(N), NS is N+1, retract(number_squares(N)), assert(number_squares(NS)).
add_shape('C') :- number_circles(N), NC is N+1, retract(number_circles(N)), assert(number_circles(NC)). 
add_shape(' ').

%Removes a colour/shape to the colour/shape counter
remove_colour_shape(Colour, Shape) :- remove_colour(Colour), remove_shape(Shape).
remove_colour('B') :- number_whites(N), NW is N-1, retract(number_whites(N)), assert(number_whites(NW)).
remove_colour('P') :- number_blacks(N), NB is N-1, retract(number_blacks(N)), assert(number_blacks(NB)).
remove_colour(' ').
remove_shape('Q') :- number_squares(N), NS is N-1, retract(number_squares(N)), assert(number_squares(NS)).
remove_shape('C') :- number_circles(N), NC is N-1, retract(number_circles(N)), assert(number_circles(NC)). 
remove_shape(' ').

%Tiles sinked counter - used to check win condition
sink_count(Player) :- sink_streak(Player,Streak), !, NStreak is Streak+1, retract(sink_streak(_, Streak)), assert(sink_streak(Player,NStreak)).
sink_count(Player) :- sink_streak(OPlayer,_), Player \= OPlayer, !, retract(sink_streak(_,_)), assert(sink_streak(Player, 1)).

%Change current Player
change_player :- retract(current_player('white')), assert(current_player('black')). 
change_player :- retract(current_player('black')), assert(current_player('white')). 

%Change pass
increment_pass(Player) :- retract(number_pass(Player, Pass)), NPass is Pass + 1, assert(number_pass(Player, NPass)).
reset_pass(Player) :- set_pass(Player, 0).
set_pass(Player, Pass) :- retract(number_pass(Player, _)), assert(number_pass(Player, Pass)).

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
replace_board(X1,Y1,X2,Y2,0) :- change_tile(X1,Y1,[' ','B','C']), change_tile(X2,Y2,[' ','P','Q']).
replace_board(X1,Y1,X2,Y2,1) :- change_tile(X1,Y1,[' ','P','C']), change_tile(X2,Y2,[' ','B','Q']).
replace_board(X1,Y1,X2,Y2,2) :- change_tile(X1,Y1,[' ','B','Q']), change_tile(X2,Y2,[' ','P','C']).
replace_board(X1,Y1,X2,Y2,3) :- change_tile(X1,Y1,[' ','P','Q']), change_tile(X2,Y2,[' ','B','C']).

%Randomize board
randomize(N) :- random(0,4,N).

%Asks the type of board
ask_board :- write('Please state the board you want (major/minor): '), read(X), create_board(X), !.

%After a winner is found, end the game
end_game(Winner) :- nl, write('Player '), write(Winner), write(' has won the game!'), board_length(Length), purge_database(Length).

% Player 1 picks the towers
pick_towers :-
	once(pick_tower1),
	once(pick_tower2),
	once(pick_tower3),
	once(pick_tower4).

validate_pick_tower(Character, Number, X, Y) :-
	char_code(Character,Charcode), Y is Charcode-97, X is Number-1. 

pick_tower1 :- display_board,
	write('Player 1: State the vertical coordinate of the first white tower: (Ex: a.)'),read(Character),
	write('State the horizontal coordinate of the first white tower: (Ex: 1.)'), read(Number),
	validate_pick_tower(Character, Number, X, Y), insert_tower(X, Y, 'L'), !.
pick_tower1 :- write('Invalid tower placement!'), nl, pick_tower1.

pick_tower2 :- display_board,
	write('Player 1: State the vertical coordinate of the second white tower: (Ex: a.)'), read(Character),
	write('State the horizontal coordinate of the second white tower: (Ex: 1.)'), read(Number),
	validate_pick_tower(Character, Number, X, Y), insert_tower(X, Y, 'L'), !.
pick_tower2 :- write('Invalid tower placement!'), nl, pick_tower2.


pick_tower3 :- display_board,
	write('Player 1: State the vertical coordinate of the first black tower: (Ex: a.)'), read(Character),
	write('State the horizontal coordinate of the first black tower: (Ex: 1.)'), read(Number),
	validate_pick_tower(Character, Number, X, Y), insert_tower(X, Y, 'T'), !.
pick_tower3 :- write('Invalid tower placement!'), nl, pick_tower3.

pick_tower4 :- display_board,
	write('Player 1: State the vertical coordinate of the second black tower: (Ex: a.)'), read(Character),
	write('State the horizontal coordinate of the second black tower: (Ex: 1.)'), read(Number),
	validate_pick_tower(Character, Number, X, Y), insert_tower(X, Y, 'T'), !.
pick_tower4 :- write('Invalid tower placement!'), nl, pick_tower4.

% Player 2 picks the colour
pick_colour :- display_board, write('Player 2: Choose your colour. From now on you will be identified with your colour (white/black): '), read(Colour), colour_picked(Colour).

%Random tower picker for the cvc mode.
randomize_towers :- board_length(Length), randomize_light_tower_1(Length), randomize_light_tower_2(Length), randomize_dark_tower_1(Length), randomize_dark_tower_2(Length).
randomize_light_tower_1(Length) :- repeat, random(0,Length, XIndex), random(0,Length, YIndex), insert_tower(XIndex,YIndex,'L').
randomize_light_tower_2(Length) :- repeat, random(0,Length, XIndex), random(0,Length, YIndex), insert_tower(XIndex,YIndex,'L').
randomize_dark_tower_1(Length) :- repeat, random(0,Length, XIndex), random(0,Length, YIndex), insert_tower(XIndex,YIndex,'T').
randomize_dark_tower_2(Length) :- repeat, random(0,Length, XIndex), random(0,Length, YIndex), insert_tower(XIndex,YIndex,'T').


% Game cycle
game_cycle(Winner) :- repeat, once(make_play), check_winning_condition(Winner).
game_cycle_cvp(Winner) :- repeat, once(make_play_cvp), check_winning_condition(Winner).
game_cycle_cvc(Winner) :- repeat, once(make_play_cvc), check_winning_condition(Winner).

% Play time!
colour_picked('white') :- write('White: Your turn to play\n'), display_board. 
colour_picked('w') :- write('White: Your turn to play\n'), display_board. 
colour_picked('black') :- write('White: Your turn to play\n'), display_board. 
colour_picked('b') :- write('White: Your turn to play\n'), display_board. 

% Make play

make_play :- display_board, current_player(Player), write(Player), write(': Your turn to play'), nl, write('Make your move (slide/sink/movetower/pass): '), read(Move), make_play_aux(Move).
make_play_aux(Move) :- Move == 'sink', sink_tile.
make_play_aux(Move) :- Move == 'movetower', move_tower.
make_play_aux(Move) :- Move == 'slide', slide_tile.
make_play_aux(Move) :- Move == 'pass', pass.
make_play_aux(_) :- write('Invalid move!'), nl, nl.

make_play_cvp :- current_player(Player), bot_colour(Player), make_bot_play(Player).
make_play_cvp :- current_player(Player), \+ bot_colour(Player), make_play.

make_play_cvc :- current_player(Player), display_board, write(Player), write(': '), make_bot_play(Player).

make_bot_play(Player) :- bot_action(1, Player, Action), make_bot_move(Action).
make_bot_move(['move',StartX,StartY,X,Y]) :- move_tower_aux(StartX,StartY,X,Y), nl, nl, write('The bot moved a tower from '), print_bot_play_coordinates(StartX, StartY), write(' to '),  print_bot_play_coordinates(X, Y), nl, nl.
make_bot_move(['pass']) :- pass.
make_bot_move(['slide',StartX, StartY, X, Y]) :- slide_tile_aux(StartX,StartY,X,Y), nl, nl, write('The bot slided a tile from '), print_bot_play_coordinates(StartX, StartY), write(' to '),  print_bot_play_coordinates(X, Y), nl, nl.
make_bot_move(['sink',X,Y]) :- sink_tile_aux(X,Y), nl, nl, write('The bot sinked a tile in '), print_bot_play_coordinates(X, Y), nl, nl.

% Prints on screen coordinates with the format (Character,Number)
print_bot_play_coordinates(X, Y) :- Charcode is 97 + X, char_code(Character, Charcode), Number is Y + 1, write('('), write(Character), write(' '), write(Number), write(')').

% Treat each play individually
slide_tile :- 	write('\nState the vertical coordinate of the tile you want to slide: (Ex: a.)'), read(Character),
write('\nState the horizontal coordinate of the tile you want to slide: (Ex: 1.)'), read(Number),
write('\nState the vertical coordinate of the tile you want to put the tile in: (Ex: a.)'), read(NCharacter),
write('\nState the horizontal coordinate of the tile you want to put the tile in: (Ex: 1.)'), read(NNumber),
char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1,
char_code(NCharacter,NCharcode), write('\n'), NY is NCharcode-97, NX is NNumber-1,
slide_tile_aux(X,Y,NX,NY).

sink_tile :-	write('\nState the vertical coordinate of the tile you want to remove: (Ex: a.)'), read(Character),
write('\nState the horizontal coordinate of the tile you want to remove: (Ex: 1.)'), read(Number),
char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1, write(' '),
sink_tile_aux(X,Y).

move_tower :- 	write('\nState the vertical coordinate of the tower you want to move: (Ex: a.)'), read(Character),
write('\nState the horizontal coordinate of the tower you want to move: (Ex: 1.)'), read(Number),
write('\nState the vertical coordinate of the tower you want to move the tower to: (Ex: a.)'), read(NCharacter),
write('\nState the horizontal coordinate of the tower you want to move the tile to: (Ex: 1.)'), read(NNumber),
char_code(Character,Charcode), write('\n'), Y is Charcode-97, X is Number-1,
char_code(NCharacter,NCharcode), write('\n'), NY is NCharcode-97, NX is NNumber-1,
move_tower_aux(X,Y,NX,NY).

slide_tile_aux(X,Y,NX,NY) :- valid_slide(X,Y,NX,NY), !, board_cell(X,Y,Elem), change_tile(NX,NY,Elem), change_tile(X,Y,[' ', ' ', ' ']), current_player(Player), reset_pass(Player), once(change_player).
slide_tile_aux(_,_,_,_) :- write('Invalid move!'), nl, nl.
sink_tile_aux(X,Y) :- valid_sink(X,Y), !, remove_tile(X, Y), current_player(Player), sink_count(Player), reset_pass(Player), once(change_player).
sink_tile_aux(_,_) :- write('Invalid move!'), nl, nl.
move_tower_aux(X,Y,NX,NY) :- valid_move(X,Y,NX,NY), !, board_cell(X,Y,[Tower|_]), insert_tower(NX, NY, Tower), remove_tower(X,Y), current_player(Player), reset_pass(Player), once(change_player).
move_tower_aux(_,_,_,_) :- write('Invalid move!'), nl, nl.
pass :- current_player(Player), increment_pass(Player), once(change_player).

player_tower('white', 'L').
player_tower('black', 'T').

% Check end game condition
check_winning_condition(Winner) :- sink_streak(Winner, 4).
check_winning_condition(Winner) :- completed_island(Player1), completed_island(Player2), Player1 \= Player2, !, resolve_initiative(Winner).
check_winning_condition(Winner) :- completed_island(Winner), !.
check_winning_condition(Winner) :- number_pass(Player1, 1), number_pass(Player2, 1), Player1 \= Player2, !, resolve_initiative(Winner).
check_winning_condition(Winner) :- number_pass(_, 4), number_pass(Winner, 0), !. 

completed_island('white') :- completed_light_island.
completed_island('white') :- completed_circle_island.
completed_island('black') :- completed_dark_island.
completed_island('black') :- completed_square_island.

resolve_initiative(Winner) :- sink_streak(Winner,_).

completed_dark_island :- board_cell(X,Y,[_,'P',_]), dark_island(X,Y,Island), number_blacks(N), length(Island, N).
completed_light_island :- board_cell(X,Y,[_,'B',_]), light_island(X,Y,Island), number_whites(N), length(Island, N).
completed_square_island :- board_cell(X,Y,[_,_,'Q']), square_island(X,Y,Island), number_squares(N), length(Island, N).
completed_circle_island :- board_cell(X,Y,[_,_,'C']), circle_island(X,Y,Island), number_circles(N), length(Island, N).

valid_slide(X, Y, FinalX, FinalY) :-
	current_player(Player), valid_slide(X, Y, FinalX, FinalY, Player).
valid_slide(X, Y, FinalX, FinalY, Player) :-
	board_cell(X,Y,[Tower,_,_]), player_tower(Player,Tower),
	slidable_tiles(X,Y,Tiles), member([FinalX,FinalY], Tiles).

	valid_move(X,Y,NX,NY) :-
	current_player(Player), valid_move(X, Y, NX, NY, Player).
valid_move(X,Y,NX,NY, Player) :-
	board_cell(X,Y,[Tower,_,_]), player_tower(Player,Tower),
	board_cell(NX,NY, [' ',_,_]), valid_move_aux(X,Y,NX,NY).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['L','B',_]), light_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['L',_,'C']), circle_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['T','P',_]), dark_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['T',_,'Q']), square_island(X,Y,Island), member([NX,NY],Island).

valid_sink(X, Y) :-
	current_player(Player), valid_sink(X, Y, Player).
valid_sink(X, Y, Player) :-
	player_tower(Player, Tower), tower_positions(Tower, [[X1, Y1], [X2, Y2]]),
	sinkable_tiles(X1, Y1, Tiles1), sinkable_tiles(X2, Y2, Tiles2),
	append(Tiles1, Tiles2, Sinkable), member([X,Y], Sinkable).


connected_board :- 
	board_cell(X, Y, [_, 'P', _]), reachable_tiles(X, Y, Tiles),
	length(Tiles, L), number_blacks(B), number_whites(W), !, L is B + W.

reachable_tiles(X, Y, Tiles) :- reachable_tiles_aux([[X,Y]],[], Tiles).
reachable_tiles_aux([],_, []).
reachable_tiles_aux([Next|T], Visited, Reachable) :- member(Next, Visited), !, reachable_tiles_aux(T, Visited, Reachable).
reachable_tiles_aux([[X, Y]|T], Visited, [[X, Y]|Reachable]) :- board_cell(X, Y, Cell), Cell \= [' ', ' ', ' '], !,
neighbour_cells(X,Y,Neighbours), append(T, Neighbours, NT),
reachable_tiles_aux(NT, [[X,Y]|Visited], Reachable).
reachable_tiles_aux([Next|T], Visited, Reachable) :- reachable_tiles_aux(T, [Next|Visited], Reachable).

%neighbour positions
neighbour_cells(X, Y, Neighbours) :- 
NX is X + 1, PX is X - 1, NY is Y + 1, PY is Y - 1,
Alt1 = [NX, Y], Alt2 = [PX, Y], Alt3 = [X, NY], Alt4 = [X, PY],
cells_only([Alt1, Alt2, Alt3, Alt4], Neighbours).

cells_only([],[]).
cells_only([[X, Y]|T1], [[X,Y]|T2]) :- board_cell(X,Y,_), !, cells_only(T1, T2).
cells_only([[X,Y]|T1], T2) :- \+ board_cell(X, Y, _), !, cells_only(T1,T2).

%searching islands
dark_island(X, Y, Island) :- board_cell(X,Y,[_,'P',_]), !, dark_island_search([[X,Y]], [], Island).
dark_island(X, Y, []) :- \+ board_cell(X,Y,[_,'P',_]), !.
dark_island_search([], _, []).
dark_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, dark_island_search(T,Visited,Island).
dark_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,'P',_]), !,
neighbour_cells(X,Y,Neighbours), append(T, Neighbours, NT),
dark_island_search(NT, [[X,Y]|Visited], Island).
dark_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,'P',_]), !,
dark_island_search(T,[[X,Y]|Visited], Island).
	
light_island(X, Y, Island) :- board_cell(X,Y,[_,'B',_]), !, light_island_search([[X,Y]], [], Island).
light_island(X, Y, []) :- \+ board_cell(X,Y,[_,'B',_]), !.
light_island_search([], _, []).
light_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, light_island_search(T,Visited,Island).
light_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,'B',_]), !,
neighbour_cells(X,Y,Neighbours), append(T, Neighbours, NT),
light_island_search(NT, [[X,Y]|Visited], Island).
light_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,'B',_]), !,
light_island_search(T,[[X,Y]|Visited], Island).

circle_island(X, Y, Island) :- board_cell(X,Y,[_,_,'C']), !, circle_island_search([[X,Y]], [], Island).
circle_island(X, Y, []) :- \+ board_cell(X,Y,[_,_,'C']), !.
circle_island_search([], _, []).
circle_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, circle_island_search(T,Visited,Island).
circle_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,_,'C']), !,
neighbour_cells(X,Y,Neighbours), append(T, Neighbours, NT),
circle_island_search(NT, [[X,Y]|Visited], Island).
circle_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,_,'C']), !,
circle_island_search(T,[[X,Y]|Visited], Island).
	
square_island(X, Y, Island) :- board_cell(X,Y,[_,_,'Q']), !, square_island_search([[X,Y]], [], Island).
square_island(X, Y, []) :- \+ board_cell(X,Y,[_,_,'Q']), !.
square_island_search([], _, []).
square_island_search([Tile|T], Visited, Island) :- member(Tile, Visited), !, square_island_search(T,Visited,Island).
square_island_search([[X,Y]|T], Visited, [[X,Y]|Island]) :- \+ member([X,Y],Visited), board_cell(X,Y,[_,_,'Q']), !,
neighbour_cells(X,Y,Neighbours), append(T, Neighbours, NT),
square_island_search(NT, [[X,Y]|Visited], Island).
square_island_search([[X,Y]|T], Visited, Island) :- \+ member([X,Y], Visited), \+ board_cell(X,Y,[_,_,'Q']), !,
square_island_search(T,[[X,Y]|Visited], Island).

%sinkable tiles

tower('L').
tower('T').
tower_positions(Tower, [[X1, Y1], [X2, Y2]]) :- tower(Tower), board_cell(X1,Y1,[Tower,_,_]), board_cell(X2,Y2,[Tower,_,_]), [X1,Y1] \= [X2,Y2].

sinkable_tiles(X,Y,Tiles) :- board_cell(X,Y,[Tower,_,_]), Tower \= ' ',!, neighbour_cells(X,Y,Neighbours), free_edges(Neighbours,FreeEdges),
empty_tiles(FreeEdges, EmptyTiles), sinkable_tiles_valid(EmptyTiles, Tiles).
sinkable_tiles(X,Y,[]) :- board_cell(X, Y, [' ',_,_]).

free_edges([], []).
free_edges([[X,Y]|Tiles], MoreTiles) :- \+board_cell(X,Y,_), free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- neighbour_cells(X, Y, Neighbours), length(Neighbours, NumNeighbours), NumNeighbours \= 4, !, free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- neighbour_cells(X, Y, Neighbours), member([X1, Y1], Neighbours), board_cell(X1, Y1, [' ',' ',' ']), !,free_edges(Tiles, MoreTiles).
free_edges([_|Tiles], MoreTiles) :- free_edges(Tiles,MoreTiles).

empty_tiles([],[]).
empty_tiles([[X,Y]|Tiles], EmptyTiles) :- board_cell(X, Y, [' ',' ',' ']), !, empty_tiles(Tiles, EmptyTiles).
empty_tiles([[X,Y]|Tiles], [[X,Y]|EmptyTiles]) :- board_cell(X, Y, [' ', _, _]), !, empty_tiles(Tiles, EmptyTiles).
empty_tiles([[X,Y]|Tiles], EmptyTiles) :- \+ board_cell(X, Y, [' ', _, _]), !, empty_tiles(Tiles, EmptyTiles).

%% change colour and shape counts
sinkable_tiles_valid([],[]).
sinkable_tiles_valid([[X,Y]|Tiles],ValidTiles) :- sinkable_tiles_valid(Tiles, VTiles), board_cell(X,Y,Cell), remove_tile(X, Y),
(connected_board -> ValidTiles = [[X,Y]|VTiles]; ValidTiles = VTiles), add_tile(X, Y, Cell).

%searching slidable positions
slidable_tiles(X, Y, Tiles) :-
	board_cell(X, Y, [Tower,_,_]), Tower \= ' ',
	neighbour_cells(X,Y,Neighbours), slidable_tiles_search(Neighbours, [[X,Y]], PTiles),
	slidable_tiles_valid(X, Y, PTiles, Tiles).

slidable_tiles_search([],_,[]).
slidable_tiles_search([Tile|T], Visited, PTiles) :- member(Tile, Visited), !, slidable_tiles_search(T, Visited, PTiles).
slidable_tiles_search([[X,Y]|T], Visited, [[X,Y]|PTiles]) :- \+ member([X,Y], Visited), board_cell(X, Y, [' ',' ',' ']), within_board_limits(X,Y), !,
neighbour_cells(X, Y, Neighbours), append(T, Neighbours, NT),
slidable_tiles_search(NT, [[X,Y]|Visited], PTiles).
slidable_tiles_search([[X,Y]|T], Visited, PTiles) :- \+ member([X,Y], Visited), !,
slidable_tiles_search(T,[[X,Y]|Visited], PTiles).

slidable_tiles_valid(_,_,[], []).
slidable_tiles_valid(StartX, StartY, [[X,Y]|PTiles],Tiles) :- slidable_tiles_valid(StartX, StartY, PTiles, NTiles),
board_cell(StartX,StartY,Cell), remove_tile(StartX, StartY), add_tile(X,Y,Cell),
(connected_board -> Tiles = [[X,Y]|NTiles]; Tiles = NTiles), add_tile(StartX, StartY, Cell), remove_tile(X, Y). 
	
within_board_limits(X, Y) :- board_limits(MinX, MaxX, MinY, MaxY), X >= MinX, X =< MaxX, Y >= MinY, Y =< MaxY.

tiles_in_X(X, Tiles) :- var(Tiles), tiles_in_X_aux(X, 0, Tiles).
tiles_in_X_aux(_, Y, []) :- board_length(Y).
tiles_in_X_aux(X, Y, [[X,Y]|Tiles]) :- board_cell(X, Y, [_,Colour,_]), Colour \= ' ', !, NY is Y + 1, tiles_in_X_aux(X,NY, Tiles).
tiles_in_X_aux(X, Y, Tiles) :- NY is Y + 1, tiles_in_X_aux(X, NY,Tiles).

tiles_in_Y(Y, Tiles) :- var(Tiles), tiles_in_Y_aux(0, Y, Tiles).
tiles_in_Y_aux(X, _, []) :- board_length(X).
tiles_in_Y_aux(X, Y, [[X,Y]|Tiles]) :- board_cell(X, Y, [_,Colour,_]), Colour \= ' ', !, NX is X + 1, tiles_in_Y_aux(NX,Y, Tiles).
tiles_in_Y_aux(X, Y, Tiles) :- NX is X + 1, tiles_in_Y_aux(NX, Y,Tiles).

board_limits(MinX, MaxX, MinY, MaxY) :- min_x(MinX), max_x(MaxX), min_y(MinY), max_y(MaxY).
min_x(MinX) :- min_x_aux(0, MinX).
min_x_aux(X, MinX) :- once(tiles_in_X(X, Tiles)), Tiles = [], !,NX is X + 1, min_x_aux(NX, MinX).
min_x_aux(X, MinX) :- var(MinX), MinX = X.

max_x(MaxX) :- board_length(Length), X is Length - 1, max_x_aux(X,MaxX).
max_x_aux(X, MaxX) :- once(tiles_in_X(X, Tiles)), Tiles = [], !, PX is X - 1, max_x_aux(PX, MaxX).
max_x_aux(X, MaxX) :- var(MaxX), MaxX = X.

min_y(MinY) :- min_y_aux(0, MinY).
min_y_aux(Y, MinY) :- once(tiles_in_Y(Y, Tiles)), Tiles = [], !, NY is Y + 1, min_y_aux(NY, MinY).
min_y_aux(Y, MinY) :- var(MinY), MinY = Y.

max_y(MaxY) :- board_length(Length), Y is Length - 1, max_y_aux(Y,MaxY).
max_y_aux(Y, MaxY) :- once(tiles_in_Y(Y, Tiles)), Tiles = [], !, PY is Y - 1, max_y_aux(PY, MaxY).
max_y_aux(Y, MaxY) :- var(MaxY), MaxY = Y.



%Available Actions
available_actions(Player, Actions) :-
	pass_actions(Player, PassActions), move_actions(Player, MoveActions),
	slide_actions(Player, SlideActions), sink_actions(Player, SinkActions),
	append(PassActions, MoveActions, L1), append(L1, SlideActions, L2),
	append(L2, SinkActions, Actions).

pass_actions(_, [['pass']]).

move_actions('white', MoveActions) :-
	player_tower('white', Tower), tower_positions(Tower, [[X1,Y1],[X2,Y2]]), !,
	light_island(X1,Y1,LightIsland1), circle_island(X1,Y1,CircleIsland1),
	append(LightIsland1, CircleIsland1, Island1), list_moves(X1, Y1, Island1, MoveActions1),
	light_island(X2,Y2,LightIsland2), circle_island(X2,Y2,CircleIsland2),
	append(LightIsland2, CircleIsland2, Island2), list_moves(X2, Y2, Island2, MoveActions2),
	append(MoveActions1, MoveActions2, MoveActions).
move_actions('black', MoveActions) :-
	player_tower('black', Tower), tower_positions(Tower, [[X1,Y1],[X2,Y2]]), !,
	dark_island(X1,Y1,DarkIsland1), square_island(X1,Y1,SquareIsland1),
	append(DarkIsland1, SquareIsland1, Island1), list_moves(X1, Y1, Island1, MoveActions1),
	dark_island(X2,Y2,DarkIsland2), square_island(X2,Y2,SquareIsland2),
	append(DarkIsland2, SquareIsland2, Island2), list_moves(X2, Y2, Island2, MoveActions2),
	append(MoveActions1, MoveActions2, MoveActions).
remove_invalid_moves(_, _, [], []).
remove_invalid_moves(StartX, StartY, [[StartX, StartY]|T], T1) :-
	!, remove_invalid_moves(StartX, StartY, T, T1).
remove_invalid_moves(StartX, StartY, [[X, Y]|T], T1) :-
	board_cell(X, Y, [' ',_,_]), !, remove_invalid_moves(StartX, StartY, T, T1).
remove_invalid_moves(StartX, StartY, [[X, Y]|T], [[X, Y]|T1]) :-
	!, remove_invalid_moves(StartX, StartY, T, T1).
list_moves(_, _, [], []).
list_moves(StartX, StartY, [[X, Y]|EndList], [['move',StartX,StartY,X,Y]|MoveList]) :-
	list_moves(StartX, StartY, EndList, MoveList).

slide_actions(Player, SlideActions) :- 
	player_tower(Player, Tower), tower_positions(Tower, [[X1,Y1],[X2,Y2]]), !,
	slidable_tiles(X1,Y1,Slides1), list_slides(X1, Y1, Slides1, SlideActions1),
	slidable_tiles(X2,Y2,Slides2), list_slides(X2, Y2, Slides2, SlideActions2),
	append(SlideActions1, SlideActions2, SlideActions).
list_slides(_, _, [], []).
list_slides(StartX, StartY, [[X,Y]|EndList], [['slide',StartX, StartY, X, Y]|SlideList]) :-
	list_slides(StartX, StartY, EndList, SlideList).

sink_actions(Player, SinkActions) :-
	player_tower(Player, Tower), tower_positions(Tower, [[X1,Y1],[X2,Y2]]), !,
	sinkable_tiles(X1, Y1, Sinks1), list_sinks(Sinks1, SinkActions1),
	sinkable_tiles(X2, Y2, Sinks2), list_sinks(Sinks2, SinkActions2),
	append(SinkActions1, SinkActions2, SinkActions).
list_sinks([],[]).
list_sinks([[X,Y]|Tiles], [['sink',X,Y]|SinkList]) :- list_sinks(Tiles, SinkList).

%Evaluation funtions
evaluate_board(Player, Score) :- number_tiles_criteria(Score1),
sinkable_tiles_criteria(Score2), islands_criteria(Score3),
sink_streak_criteria(Score4), winning_criteria(Score5),
Calc is Score1 + Score2 + Score3 + Score4 + Score5,
(Player \= 'white' -> Score = Calc; Score is -Calc).

number_tiles_criteria(Score) :- number_blacks(B), number_whites(W), number_circles(C), number_squares(S),
Score is W + C - (B + S).

sinkable_tiles_criteria(Score) :- 	player_tower('white', WTower), tower_positions(WTower, [[WX1,WY1],[WX2,WY2]]),
sinkable_tiles(WX1, WY1, WSinks1), sinkable_tiles(WX2, WY2, WSinks2),
append(WSinks1, WSinks2, WSinks), sinkable_tiles_criteria_aux(WSinks, WScore),
player_tower('black', BTower), tower_positions(BTower, [[BX1,BY1],[BX2,BY2]]),
sinkable_tiles(BX1, BY1, BSinks1), sinkable_tiles(BX2, BY2, BSinks2),
append(BSinks1, BSinks2, BSinks), sinkable_tiles_criteria_aux(BSinks, BScore),
Score is WScore - BScore.

sinkable_tiles_criteria_aux([], 0).
sinkable_tiles_criteria_aux([[X,Y]|Sinks], Score) :-	sinkable_tiles_criteria_aux(Sinks, Score1),
sinkable_tiles_score(X, Y, Score2),
Score is Score1 + Score2.
sinkable_tiles_score(X, Y, Score) :- 	board_length(Length), MaxDist is Length/sqrt(2),
CX is truncate(Length / 2), CY = CX,
Dist is sqrt((CX - X)^2+(CY - Y)^2),
Score is (10 * (MaxDist - Dist) / MaxDist).

islands_criteria(Score) :- 	island_score('white', WScore), island_score('black', BScore),
Score is WScore - BScore.

island_score('white', Score) :-	player_tower('white', Tower), tower_positions(Tower,[[X1,Y1],[X2,Y2]]),
light_island(X1,Y1,LightIsland1), circle_island(X1,Y1,CircleIsland1),
light_island(X2,Y2,LightIsland2), circle_island(X2,Y2,CircleIsland2),
number_whites(Whites), number_circles(Circles),
length(LightIsland1, Length1), length(LightIsland2, Length2), length(CircleIsland1, Length3), length(CircleIsland2, Length4),
Score is 50*(((Length1 + Length2)/ Whites) + ((Length3+Length4) / Circles)).
	
island_score('black', Score) :-	player_tower('black', Tower), tower_positions(Tower,[[X1,Y1],[X2,Y2]]),
dark_island(X1,Y1,DarkIsland1), square_island(X1,Y1,SquareIsland1),
dark_island(X2,Y2,DarkIsland2), square_island(X2,Y2,SquareIsland2),
number_blacks(Blacks), number_squares(Squares),
length(DarkIsland1, Length1), length(DarkIsland2, Length2), length(SquareIsland1, Length3), length(SquareIsland2, Length4),
Score is 50*(((Length1 + Length2)/ Blacks) + ((Length3+Length4) / Squares)).
	
sink_streak_criteria(Score) :- sink_streak('white', Sinks), !, Score is 20 * Sinks.
sink_streak_criteria(Score) :- sink_streak('black', Sinks), !, Score is -20 * Sinks.

winning_criteria(Score) :- check_winning_condition(Winner), !, 
(Winner = 'white' -> Score is 10000; Score is -10000).
winning_criteria(Score) :- Score is 0.
							

bot_pick_colour(Colour) :- evaluate_board('white', Score), bot_pick_colour_aux(Score, Colour).
bot_pick_colour_aux(Score, Colour) :- (Score < 0 -> Colour = 'black'; Colour = 'white').
							
bot_action(0, Player, Action) :- available_actions(Player, Actions), length(Actions, Length),
 random(0, Length, Index), nth0(Index,Actions, Action).
 
bot_action(1, Player, Action) :- available_actions(Player, Actions), bot_action_helper(Player, Actions, -10000, [], Action).

bot_action_helper(_,[], _, BestAction, BestAction).
bot_action_helper(Player, [Action|Actions], BestScore, BestAction, SelectedAction) :-
evaluate_action(Player, Action, Score),
(Score > BestScore -> (NScore = Score, NAction = Action); (NScore = BestScore, NAction = BestAction)),
bot_action_helper(Player, Actions, NScore, NAction, SelectedAction).

evaluate_action(Player, ['pass'], Score) :-
	number_pass(Player, NumPass), N is NumPass + 1, set_pass(Player, N),
	evaluate_board(Player, Score), set_pass(Player, NumPass).

evaluate_action(Player, ['move', X, Y, NX, NY], Score) :-
	player_tower(Player, Tower), remove_tower(X, Y), insert_tower(NX, NY, Tower),
	evaluate_board(Player, Score), remove_tower(NX, NY), insert_tower(X, Y, Tower).
	
evaluate_action(Player, ['slide', X, Y, NX, NY], Score) :-
	board_cell(X, Y, Cell), remove_tile(X,Y), add_tile(NX, NY, Cell),
	evaluate_board(Player, Score), remove_tile(NX, NY), add_tile(X, Y, Cell).
	
evaluate_action(Player, ['sink', X, Y], Score) :-
	board_cell(X, Y, Cell), remove_tile(X, Y),
	evaluate_board(Player, Score), add_tile(X, Y, Cell).