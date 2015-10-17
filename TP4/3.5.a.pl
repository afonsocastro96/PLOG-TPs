membro(X,[X|_]).
membro(X,[H|T]) :- H \= X, membro(X,T).
