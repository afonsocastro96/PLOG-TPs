ordenada([H]).
ordenada([]).
ordenada([A,B]) :- A =< B.
ordenada([A,B|T]) :- A =< B, ordenada([B|T]). 