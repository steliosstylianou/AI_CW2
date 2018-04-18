% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A)
  ; otherwise       -> find_identity_o(A)
  ).

generate_actor_link_list(X):-
  writeln("Generating Actor's links"),
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
  my_agent(Agent),
  query_world(agent_current_position,[Agent,P]),
  writeln("Please wait while finding Charging Station locations"),
  find_charging_stations(ChargingLocations,P,[]),
  writeln("Please wait while finding Oracle locations"),
  find_oracles(OraclesLocations,P,[]),
  writeln("Finished"),
  find_myself(A,ActorList,[],ChargingLocations,OraclesLocations,P).

find_charging_stations(ChargingLocations,P,Draft):-
  length(Draft,2),
  ChargingLocations = Draft.

find_charging_stations(ChargingLocations,P,Draft):-
  Task = find(c(C)),
  solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
  map_adjacent(NewPos,ChargingPos,c(C)),
  \+ memberchk((ChargingPos,c(C)),Draft),
  find_charging_stations(ChargingLocations,NewPos,[(ChargingPos,c(C))|Draft]),!.

find_oracles(OraclesLocations,P,Draft):-
  explore_grid([[c(0,0,P),P]],0,R,Cost,[P],OraclesLocations,Draft),!.

generate_cost_list(P,Locations,CostList,Draft):-
  Locations = [],
  CostList = Draft.

generate_cost_list(P,Locations,CostList,Draft):-
  Locations = [Head|Tail],
  Head = (ChargingPos, ID),
  map_distance(P,ChargingPos,Cost),
  append(Draft,[(Cost,ChargingPos,ID)],NewDraft),
  generate_cost_list(P,Tail,CostList,NewDraft),!.

generate_cost_adj(P,AdjPositions,CostList,Draft):-
    AdjPositions = [],
    CostList = Draft.

generate_cost_adj(P,AdjPositions,CostList,Draft):-
    AdjPositions = [AdjPos|Tail],
    map_distance(P,AdjPos,Cost),
    append(Draft,[(Cost,AdjPos)],NewDraft),
    generate_cost_adj(P,Tail,CostList,NewDraft),!.

find_lower_cost(Goal, P, Locations,Visited):-
  generate_cost_list(P,Locations,CostList,[]),
  sort(CostList,Sorted),
  Locations = [(_,ID)|_],
  (ID = o(O) -> find_lowest_unvisited(Sorted,Visited,NewGoal), NewGoal = (Cost,GoalPos,NewID) ;
   ID = c(C) -> Sorted = [(Cost,GoalPos,NewID)|_]),
  setof(NewPos, map_adjacent(GoalPos,NewPos,empty), Positions),
  find_lower_adj(FinalPos,P,Positions),
  Goal = (FinalPos,NewID).

find_lowest_unvisited(Sorted,Visited,NewGoal):-
  Sorted = [(Cost,GoalPos,ID)|Tail],
  (\+ memberchk(ID, Visited) -> NewGoal =  (Cost,GoalPos,ID)
  ; find_lowest_unvisited(Tail, Visited, NewGoal)).

find_lower_adj(NewPos, P, Locations):-
  generate_cost_adj(P,Locations,CostList,[]),
  sort(CostList,Sorted),
  Sorted = [(Cost,NewPos)|_].

find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations,P):-
  ActorList = [[ actor(A)|_]].

find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations,P):-
  length(VisitedOracles,10),
  writeln('Finished and I still do not know who I am').

find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations,P):-
  writeln('Iterating'),
  my_agent(Agent),
  query_world(agent_current_energy,[Agent,Energy]),
  write('Energy'),writeln(Energy),
  ( Energy < 50 ->find_lower_cost(Goal,P,ChargingLocations,VisitedOracles),
                  Goal = (GoalPos,c(C)),
                  write("Charging Goal Pos "),writeln(GoalPos),
                  write("Charging Station "),writeln(C),
                  Task = go(GoalPos),
                  solve_task(Task,CostTask),
                  NewVisitedOracles = VisitedOracles
                  ;
                  find_lower_cost(Goal,P,OraclesLocations,VisitedOracles),
                  Goal = (GoalPos,o(O)),
                  write("Oracle Goal Pos "),writeln(GoalPos),
                  write("Oracle Station "),writeln(O),
                  Task = go(GoalPos),
                  solve_task(Task,CostTask),
                  NewVisitedOracles = [o(O)|VisitedOracles]) ,

   (Goal = (GoalPos,c(C)) ->  writeln('toping up'), query_world(agent_topup_energy,[Agent,c(C)]),
                              writeln('toped up'), ActorsLeft = ActorList
   ;Goal = (GoalPos,o(O)) ->  write('Asking Oracle'), writeln(O), whosleft(ActorList,o(O),ActorsLeft),
                              write('Actors Left'),length(ActorsLeft,Le),writeln(Le)) ,
   find_myself(A,ActorsLeft,NewVisitedOracles,ChargingLocations,OraclesLocations,GoalPos),!.

write_pos(Pos):-
  Pos = p(X,Y),
  write(X), write(' '), writeln(Y).
