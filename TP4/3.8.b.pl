conta_elem(X,[],0).
conta_elem(X,[H|T],N) :- conta_elem(X,T,N).
conta_elem(X,[X|T],N) :- Y is N+1, conta_elem(X,T,Y).