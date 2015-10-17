select(X,[X|T],T).
select(X,[H|T],[H|L2]) :- X\=H, select(X,T,L2).
permutacao(L,L).
permutacao([H|T],L2) :- select(H,L2,L3), permutacao(T,L3).