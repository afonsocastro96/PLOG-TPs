substitui(_,_,[],[]).
substitui(X,Y,[X|T],[Y|U]) :- substitui(X,Y,T,U).
substitui(X,Y,[H|T],[H|U]) :- X\=H, substitui(X,Y,T,U).