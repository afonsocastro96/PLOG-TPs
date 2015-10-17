soma_lista([],0).
soma_lista([H|T],Soma) :- Y is Soma-H, soma_lista(T,Y).