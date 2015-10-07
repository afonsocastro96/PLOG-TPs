dummy_board(Board) :- Board is 
[
[[' ',' ',' '], [' ',' ',' '], [' ','B','C'], [' ','B','Q'], [' ','B','Q'], [' ',' ',' '],[' ',' ',' ']],
[[' ',' ',' '], [' ','P','Q'], [' ','P','C'], [' ','P','Q'], [' ','B','C'], [' ','P','C'],[' ',' ',' ']],
[[' ','P','C'], [' ','B','Q'], [' ','B','C'], [' ','B','Q'], [' ','P','Q'], [' ','B','C'],[' ','B','C']],
[[' ','P','Q'], [' ','P','C'], [' ','P','C'], [' ',' ',' '], [' ','B','Q'], [' ','B','Q'],[' ','B','C']],
[[' ','P','Q'], [' ','P','Q'], [' ','B','C'], [' ','P','C'], [' ','P','Q'], [' ','P','C'],[' ','B','Q']],
[[' ',' ',' '], [' ','B','Q'], [' ','P','Q'], [' ','B','C'], [' ','B','Q'], [' ','B','C'],[' ',' ',' ']],
[[' ',' ',' '], [' ',' ',' '], [' ','P','C'], [' ','P','C'], [' ','P','Q'], [' ',' ',' '],[' ',' ',' ']],
].

display_board(Board) :- display_board_aux(Board,7).

display_board_aux(Board,0) :- write_border(7).

display_board_aux(Board,N) :- N > 0, write_border(7), write_line(Board,7), Y is N-1, display_board_aux(Board,Y).

write_line(Board,0) :- write('|\n').

write_line(Board,N) :- N > 0, write('|   '), Y is N-1, write_line(Board,Y).

write_border(0) :- write('+\n').

write_border(N) :- N > 0, write('+---'), Y is N-1, write_border(Y).