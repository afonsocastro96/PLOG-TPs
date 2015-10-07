inverter(L1,L2) :- aux(L1,[],L2).
aux([],S,S).
aux([H|T],L,S) :- aux(T,[H|L],S).