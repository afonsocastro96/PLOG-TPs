delete_all_list([],L1,L1).
delete_all_list([X|T],L1,L2) :- delete_all(X,L1,L3), delete_all_list(T,L3,L2).