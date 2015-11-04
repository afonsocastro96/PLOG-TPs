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
:- dynamic winner/1.

%Database manipulation
purge_database(N) :-	N > 0, purge_database_aux(0,0), retract(board_length(N)), retract(sink_streak(_,_)), retract(current_player(_)),
						retract(number_circles(_)), retract(number_squares(_)), retract(number_blacks(_)), retract(number_whites(_)),
						(winner(_) -> retract(winner(_)); true).
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
game :- write('Please state the board you want (major/minor): '), read(X), create_board(X), pick_tower, pick_colour, game_cycle, write('\nPlayer '), winner(Player), write(Player), write(' has won the game!').
create_board(minor) :- create_database(5), randomize_board_minor.
create_board(major) :- create_database(7), randomize_board_major.

%Randomize board
randomize(N) :- random(0,4,N).

/****************

Database modifiers

*****************/

%Changes a tile's content
add_tile(X,Y,[Tower,Colour, Shape]) :- board_cell(X,Y,[' ',' ',' ']), change_tile(X, Y, [Tower,Colour,Shape]), add_colour_shape(Colour, Shape).
change_tile(X,Y,[Tower,Colour,Shape]) :- retract(board_cell(X,Y,_)), assert(board_cell(X,Y,[Tower,Colour,Shape])).
remove_tile(X, Y) :- board_cell(X, Y, [_,Colour,Shape]), change_tile(X,Y,[' ',' ',' ']), remove_colour_shape(Colour, Shape).

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
game_cycle :- repeat, once(make_play), once(change_player), check_winning_condition.

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
slide_tile(X,Y,NX,NY) :- valid_slide(X,Y,NX,NY), board_cell(X,Y,Elem), change_tile(NX,NY,Elem), change_tile(X,Y,[' ', ' ', ' ']).
sink_tile(X,Y) :- board_cell(X, Y, [' ',C,S]), !, remove_colour_shape(C,S), change_tile(X,Y,[' ', ' ', ' ']).
move_tower(X,Y,NX,NY) :- valid_move(X,Y,NX,NY), board_cell(X,Y,[Tower|_]), insert_tower(NX, NY, Tower), remove_tower(X,Y).
pass.

player_tower('white', 'L').
player_tower('black', 'T').

% Check end game condition
check_winning_condition :- sink_streak(Player, 4), assert(winner(Player)).
check_winning_condition :- board_cell(X,Y,[_,'P',_]), dark_island(X,Y,Island), number_blacks(N), length(Island, N), assert(winner('black')).
check_winning_condition :- board_cell(X,Y,[_,'B',_]), light_island(X,Y,Island), number_whites(N), length(Island, N), assert(winner('white')).
check_winning_condition :- board_cell(X,Y,[_,_,'Q']), square_island(X,Y,Island), number_squares(N), length(Island, N), assert(winner('black')).
check_winning_condition :- board_cell(X,Y,[_,_,'C']), circle_island(X,Y,Island), number_circles(N), length(Island, N), assert(winner('white')).



valid_slide(X, Y, FinalX, FinalY) :- board_cell(X,Y,[Tower,_,_]), current_player(Player), player_tower(Player,Tower),slidable_tiles(X,Y,Tiles), member([FinalX,FinalY], Tiles).
valid_move(X,Y,NX,NY) :- 	board_cell(X,Y,[Tower,_,_]), current_player(Player), player_tower(Player,Tower),
							board_cell(NX,NY, [' ',_,_]), valid_move_aux(X,Y,NX,NY).

valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['L','B',_]), light_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['L',_,'C']), circle_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['T','P',_]), black_island(X,Y,Island), member([NX,NY],Island).
valid_move_aux(X,Y,NX,NY) :- board_cell(X,Y,['T',_,'Q']), square_island(X,Y,Island), member([NX,NY],Island).

valid_sink(X, Y) :- current_player(Player), player_tower(Player, Tower), tower_positions(Tower, [[X1, Y1], [X2, Y2]]),
					sinkable_tiles(X1, Y1, Tiles1), sinkable_tiles(X2, Y2, Tiles2), append(Tiles1, Tiles2, Sinkable),
					member([X,Y], Sinkable).

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

%sinkable tiles

tower('L').
tower('T').
tower_positions(Tower, [[X1, Y1], [X2, Y2]]) :- tower(Tower), board_cell(X1,Y1,[Tower,_,_]), board_cell(X2,Y2,[Tower,_,_]), [X1,Y1] \= [X2,Y2].

sinkable_tiles(X,Y,Tiles) :- \+board_cell(X,Y,[' ',_,_]), !, neighbour_tiles(X,Y,Neighbours), free_edges(Neighbours,FreeEdges),
	empty_tiles(FreeEdges, EmptyTiles), sinkable_tiles_valid(EmptyTiles, Tiles).
sinkable_tiles(_,_,[]).

free_edges([], []).
free_edges([[X,Y]|Tiles], MoreTiles) :- \+board_cell(X,Y,_), free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- A is X+1, \+board_cell(A,Y,_), !, free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- B is X-1, \+board_cell(B,Y,_), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- C is Y+1, \+board_cell(X,C,_), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- D is X-1, \+board_cell(X,D,_), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- A is X+1, board_cell(A,Y,[' ',' ',' ']), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- B is X-1, board_cell(B,Y,[' ',' ',' ']), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- C is Y+1, board_cell(X,C,[' ',' ',' ']), !,free_edges(Tiles, MoreTiles).
free_edges([[X,Y]|Tiles], [[X,Y]|MoreTiles]) :- D is X-1, board_cell(X,D,[' ',' ',' ']), !,free_edges(Tiles, MoreTiles).
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
slidable_tiles(X, Y, Tiles) :- \+ board_cell(X, Y, [' ',_,_]),
	neighbour_tiles(X,Y,Neighbours), slidable_tiles_search(Neighbours, [[X,Y]], PTiles),
	slidable_tiles_valid(X, Y, PTiles, Tiles).

slidable_tiles_search([],_,[]).
slidable_tiles_search([Tile|T], Visited, PTiles) :- member(Tile, Visited), !, slidable_tiles_search(T, Visited, PTiles).
slidable_tiles_search([[X,Y]|T], Visited, [[X,Y]|PTiles]) :- \+ member([X,Y], Visited), board_cell(X, Y, [' ',' ',' ']), within_board_limits(X,Y), !,
	neighbour_tiles(X, Y, Neighbours), append(T, Neighbours, NT),
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
