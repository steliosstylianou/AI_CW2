% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise -> find_identity_o(A)
  ).

generate_actor_link_list(X):-
  bagof([actor(A)|L], (actor(A), generate_actor_link(A,L)), X).

generate_actor_link(A,Ls):-
  bagof(L, (wp(A,WT),wt_link(WT,L)), Ls ).

find_identity_2(A):-
  generate_actor_link_list(List),
  whatsmyname(A,List).

found_myself(A,List):-
  List = [[ actor(A)| Links]].

whatsmyname(A,List):-
  found_myself(A,List).

whatsmyname(A,List):-
  agent_ask_oracle(oscar,o(1),link,L),
  filter_actors(List,L,FilteredList),
  whatsmyname(A,FilteredList).

filter_actors(List,Link,FilteredList):-
  List = [[Actor(A)|Links] | RestActors],
  
find_identity_o(A):-
  A='Not yet implemented'.
