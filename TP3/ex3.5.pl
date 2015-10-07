membro(X,[X|L]).
membro(X,[H|T]) :- X \= H, membro(X,T).