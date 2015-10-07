nessdasdas([X|L],1,X).
nessdasdas([H|T],N,X) :- N>1,Y is N-1, nessdasdas(T,Y,X).