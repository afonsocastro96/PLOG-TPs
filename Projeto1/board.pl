dummy_board(Board) :- Board =
[[[' ',' ',' '], [' ',' ',' '], [' ','B','C'], [' ','B','Q'], [' ','B','Q'], [' ',' ',' '], [' ',' ',' ']],
 [[' ',' ',' '], [' ','P','Q'], [' ','P','C'], [' ','P','Q'], ['L','B','C'], [' ','P','C'], [' ',' ',' ']],
 [[' ','P','C'], [' ','B','Q'], ['L','B','C'], [' ','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','C'], [' ','P','C'], [' ',' ',' '], ['T','B','Q'], [' ','B','Q'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','Q'], [' ','B','C'], [' ','P','C'], [' ','P','Q'], [' ','P','C'], [' ','B','Q']],
 [[' ',' ',' '], ['T','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','Q'], [' ','B','C'], [' ',' ',' ']],
 [[' ',' ',' '], [' ',' ',' '], [' ','P','C'], [' ','P','C'], [' ','P','Q'], [' ',' ',' '], [' ',' ',' ']]].

display_board(Board) :- display_board_aux(Board).

display_board_aux([Line |Board]) :- write_border(Line), write_line(Line), display_board_aux(Board).

display_board_aux([Line |[]]) :- write_border(Line), write_line(Line), write_border(Line).

write_border([]) :- write('+\n').

write_border([Elem|Line]) :- write('+---'), write_border(Line).

write_line([]) :- write('|\n').

write_line([Elem|Line]) :- write('|'), write_elem(Elem), write_line(Line).

write_elem([Elem1|Elems]) :- write(Elem1), write_elem(Elems).

write_elem([]).