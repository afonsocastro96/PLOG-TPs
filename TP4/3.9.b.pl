elimina_duplicados([],[]).
elimina_duplicados([H|T],[H|Lista2]) :- delete_all(H,T,Lista3), elimina_duplicados(Lista3,Lista2).