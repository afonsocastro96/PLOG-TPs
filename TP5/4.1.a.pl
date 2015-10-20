ligado(a,b).
ligado(f,i).
ligado(a,c).
ligado(f,j).
ligado(b,d).
ligado(f,k).
ligado(b,e).
ligado(g,l).
ligado(b,f).
ligado(g,m).
ligado(c,g).
ligado(k,n).
ligado(d,h).
ligado(l,o).
ligado(d,i).
ligado(i,f).

dfs(Src, Dest,Sol) :- dfs(Src, Dest, Sol, []).
dfs(Src, Src, [Src], _).
dfs(Src, Dest, [Src|Sol], Visited) :- Src \= Dest, ligado(Src, X), \+ member(X, Visited),
								dfs(X, Dest, Sol, [Src|Visited]).
