% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise       -> find_identity_o(A)
  ).

generate_actor_link_list(X):-
  bagof([actor(A)|L], (actor(A), generate_actor_link(A,L)), X).

generate_actor_link(A,Ls):-
  bagof(L, (wp(A,WT),wt_link(WT,L)), Ls ).

find_identity_2(A):-
  generate_actor_link_list(List),
  whatsmyname(A,List,o(1)),!.

found_identity(A,List):-
  % Process of elimination
  List = [[ actor(A)| _]].

whatsmyname(A,List,O):-
  found_identity(A,List).

whatsmyname(A,List,O):-
  agent_ask_oracle(oscar,O,link,L),
  filter_actors(List,L,FilteredList),
  whatsmyname(A,FilteredList,O).

whosleft(List,O,FilteredList):-
  my_agent(Agent),
  query_world(agent_ask_oracle,[Agent,O,link,L]),
  filter_actors(List,L,FilteredList).



filter_actors(List,Link,FilteredList):-
  include(memberchk(Link),List, FilteredList).

find_identity_o(A):-
  generate_actor_link_list(ActorList),
  find_myself(A,ActorList,[],[]).

find_myself(A,ActorList,VisitedOracles,VisitedStations):-
  ActorList = [[ actor(A)|_ ]].

find_myself(A,ActorList,VisitedOracles,VisitedStations):-
  length(VisitedOracles,10),
  writeln('Finished and I still do not know who I am').

find_myself(A,ActorList,VisitedOracles,VisitedStations):-
  my_agent(Agent),
  query_world(agent_current_energy,[Agent,Energy]),
  query_world(agent_current_position,[Agent,P]),
  write('Energy'),writeln(Energy),
  write_pos(P),
  ( Energy < 40 -> Task = find(c(C)), solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
                  NewVisitedOracles = VisitedOracles
                  ;Task = find(o(O)), solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
                  \+ memberchk(O,VisitedOracles),!,
                  NewVisitedOracles = [O|VisitedOracles],
                  write('Asking Oracle'), writeln(O) ),
   reverse(R,[_Init|Path]),
   query_world( agent_do_moves, [Agent,Path]),
   (Task = find(c(C)) -> query_world(agent_topup_energy,[Agent,c(C)]),
                         ActorsLeft = ActorList
   ; Task= find(o(O)) -> whosleft(ActorList,o(O),ActorsLeft)),
   find_myself(A,ActorsLeft,NewVisitedOracles,VisitedStations),!.

write_pos(Pos):-
  Pos = p(X,Y),
  write(X), write(' '), writeln(Y).





%
