delete_all(X,[],[]).
delete_all(X,[X|T],L2) :- delete_all(X,T,L2).
delete_all(X,[H|T],[H|L2]) :- X \= H, delete_all(X,T,L2).