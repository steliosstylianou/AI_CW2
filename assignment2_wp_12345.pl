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
  whatsmyname(A,List),!.

found_identity(A,List):-
  List = [[ actor(A)| Links]].

whatsmyname(A,List):-
  found_identity(A,List).

whatsmyname(A,List):-
  agent_ask_oracle(oscar,o(1),link,L),
  filter_actors(List,L,FilteredList),
  whatsmyname(A,FilteredList).

filter_actors(List,Link,FilteredList):-
  include(memberchk(Link),List, FilteredList).

find_identity_o(A):-
  generate_actor_link_list(ActorList),
  find_myself(A,ActorList,[],[]).

find_myself(A,ActorList,VisitedOracles,VisitedStations):-
  length(VisitedOracles,10),
  writeln('Finished').

find_myself(A,ActorList,VisitedOracles,VisitedStations):-
  my_agent(Agent),
  query_world(agent_current_energy,[Agent,Energy]),
  query_world(agent_current_position,[Agent,P]),
  write('Energy'),writeln(Energy),
  write_pos(P),
  ( Energy < 85 -> Task = find(c(C)), solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
                  NewVisitedOracles = VisitedOracles
                  ;Task = find(o(O)), solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
                  \+ memberchk(O,VisitedOracles),!,
                  NewVisitedOracles = [O|VisitedOracles],
                  write('Asking Oracle'), writeln(O) ),
   reverse(R,[_Init|Path]),
   query_world( agent_do_moves, [Agent,Path]),
   (Task = find(c(C)) -> query_world(agent_topup_energy,[Agent,c(C)]), find_myself(A,ActorList,NewVisitedOracles,VisitedStations)
   ; Task= find(o(O)) -> find_myself(A,ActorList,NewVisitedOracles,VisitedStations) )
   .

write_pos(Pos):-
  Pos = p(X,Y),
  write(X), write(' '), writeln(Y).





%
