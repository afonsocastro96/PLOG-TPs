nesimo(1,[X|_],X).
nesimo(N,[_|T],X) :- N > 1, Y is N-1, nesimo(Y,T,X).  