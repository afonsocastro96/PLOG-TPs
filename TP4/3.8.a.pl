conta([],0).
conta([_|T],N) :- conta(T,Y),N is Y+1.