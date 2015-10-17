lista_ate(0,[]).
lista_ate(N,L) :- N > 0, Y is N-1, lista_ate(Y,[N|L]).