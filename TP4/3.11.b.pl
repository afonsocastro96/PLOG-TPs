lista_entre(N,N,[N]).
lista_entre(N1,N2,L) :- N1 < N2, Y is N2-1, lista_entre(N1,Y,[N2|L]).
lista_entre(N1,N2,[]) :- N2 < N1.