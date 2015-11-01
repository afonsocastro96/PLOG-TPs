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

dfs(Src, Dest, [Src, Dest], _) :- ligado(Src,Dest).
dfs(Src, Dest, [Src, Dest], _) :- ligado(Dest, Src).
dfs(Src, Dest, [Src|Sol], Visited) :- \+ member(Src, Visited), ligado(Src, X),
								dfs(X, Dest, Sol, [Src|Visited]).
dfs(Src, Dest, [Dest|Sol], Visited) :- \+ member(Dest, Visited), ligado(X, Src),
								dfs(X, Dest, Sol, [Src|Visited]).