major_board(Board) :- Board =
[[[' ',' ',' '], [' ',' ',' '], [' ','B','C'], [' ','B','Q'], [' ','B','Q'], [' ',' ',' '], [' ',' ',' ']],
 [[' ',' ',' '], [' ','P','Q'], [' ','P','C'], [' ','P','Q'], ['L','B','C'], [' ','P','C'], [' ',' ',' ']],
 [[' ','P','C'], [' ','B','Q'], ['L','B','C'], [' ','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','C'], [' ','P','C'], [' ',' ',' '], ['T','B','Q'], [' ','B','Q'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','Q'], [' ','B','C'], [' ','P','C'], [' ','P','Q'], [' ','P','C'], [' ','B','Q']],
 [[' ',' ',' '], ['T','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','Q'], [' ','B','C'], [' ',' ',' ']],
 [[' ',' ',' '], [' ',' ',' '], [' ','P','C'], [' ','P','C'], [' ','P','Q'], [' ',' ',' '], [' ',' ',' ']]].
 
minor_board(Board) :- Board =
[[[' ',' ',' '], [' ',' ',' '], [' ','B','C'], [' ','B','Q'], [' ','B','Q'], [' ',' ',' '], [' ',' ',' ']],
 [[' ',' ',' '], [' ','P','Q'], [' ','P','C'], [' ','P','Q'], ['L','B','C'], [' ','P','C'], [' ',' ',' ']],
 [[' ','P','C'], [' ','B','Q'], ['L','B','C'], [' ','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','C'], [' ','P','C'], [' ',' ',' '], ['T','B','Q'], [' ','B','Q'], [' ','B','C']],
 [[' ','P','Q'], [' ','P','Q'], [' ','B','C'], [' ','P','C'], [' ','P','Q'], [' ','P','C'], [' ','B','Q']],
 [[' ',' ',' '], ['T','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','Q'], [' ','B','C'], [' ',' ',' ']],
 [[' ',' ',' '], [' ',' ',' '], [' ','P','C'], [' ','P','C'], [' ','P','Q'], [' ',' ',' '], [' ',' ',' ']]].

display_board(Board) :- write_col_coords(Board), display_board_aux(Board,1).

display_board_aux([Line | Board],Number) :- Board \= [], write_border(Line), write_line(Line, Number), NextNumber is Number + 1, display_board_aux(Board,NextNumber).

display_board_aux([Line], Number) :- write_border(Line), write_line(Line, Number), write_border(Line).

write_border([_|Line]) :- write('   +---'), write_border_aux(Line).

write_border_aux([]) :- write('+\n').

write_border_aux([_|Line]) :- write('+---'), write_border_aux(Line).

write_aux_line([]) :- write('|\n').

write_aux_line([Elem|Line]) :- write('|'), write_elem(Elem), write_line(Line).

write_line(Line, Number) :- write(' '), write(Number), write(' '), write_aux_line(Line).

write_elem([Tower,Colour,Shape]) :- write(Tower), write(Colour), write(Shape).

write_col_coords([Line | Board]) :- write('   '), write_col_coords_aux(Line,65).

write_col_coords_aux([Elem],Charcode) :- char_code(Character,Charcode), write('  '), write(Character), write('\n').

write_col_coords_aux([Elem|Line],Charcode) :- Line \= [], char_code(Character,Charcode), write('  '), write(Character), write(' '), Nextchar is Charcode+1, write_col_coords_aux(Line,Nextchar). 