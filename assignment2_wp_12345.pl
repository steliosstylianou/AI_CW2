% candidate_number(12345).

% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A):-
  (part_module(2)   -> find_identity_2(A),
   part_module(4)   -> find_identity_4(A)
  ; otherwise       -> find_identity_o(A)
  ).


%Predicate generating a list of all links along with the corresponding actor
generate_actor_link_list(X):-
  writeln("Generating Actor's links"),
  bagof([actor(A)|L], (actor(A), generate_actor_link(A,L)), X).

%Generating links for one actor
generate_actor_link(A,Ls):-
  bagof(L, (wp(A,WT),wt_link(WT,L)), Ls ).

% Asking the oracle to find out the agent's identity
find_identity_2(A):-
  generate_actor_link_list(List),
  whatsmyname(A,List,o(1)),!.

% If there's one actor in the list it means we are done
found_identity(A,List):-
  % Process of elimination
  List = [[ actor(A)| _]],
  write("I am "), writeln(A).

% Predicate for recursively asking the oracle for a link and filtering
% the current links. If it succeeds it returns the actor name (A).
whatsmyname(A,List,O):-
  found_identity(A,List).

whatsmyname(A,List,O):-
  agent_ask_oracle(oscar,O,link,L),
  filter_actors(List,L,FilteredList),
  whatsmyname(A,FilteredList,O).

% Predicate for filtering the list given an actor link.
filter_actors(List,Link,FilteredList):-
  include(memberchk(Link),List, FilteredList).

% Predicate for find Identity in part 3. It generates the actor links, finds the Locations
% of charging stations and oracles using BFS and
find_identity_o(A):-
  generate_actor_link_list(ActorList),
  my_agent(Agent),
  writeln("Please wait while finding Charging Station locations"),
  query_world(agent_current_position,[Agent,P]),
  find_charging_stations(ChargingLocations,P,[]),
  writeln("Please wait while finding Oracle locations"),
  find_oracles(OraclesLocations,P,[]),
  writeln("Finished"),
  length(OraclesLocations,L),
  write("Found "), write(L), writeln(" Oracles"),
  find_myself(A,ActorList,[],ChargingLocations,OraclesLocations).

%Predicate for finding charging stations. Succeds if 2 of them are found.
find_charging_stations(ChargingLocations,P,Draft):-
  length(Draft,2),
  ChargingLocations = Draft.

find_charging_stations(ChargingLocations,P,Draft):-
  Task = find(c(C)),
  solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,NewPos,[]),
  map_adjacent(NewPos,ChargingPos,c(C)),
  \+ memberchk((ChargingPos,c(C)),Draft),
  find_charging_stations(ChargingLocations,NewPos,[(ChargingPos,c(C))|Draft]),!.

%Predicate for finding oracle locations. Succeds if 10 of them are found or
% the whole map was searched and no more oracles were found.
% Implemented using BFS on the whole grid while appending locations when they are found

find_oracles(OraclesLocations,P,Draft):-
  explore_grid([[c(0,0,P),P]],0,R,Cost,[P],OraclesLocations,Draft),!.

% Part 3: BFS for finding OraclesLocations, stopping when the list is 10 or when the whole
% map was explored but no more oracles have been found.

explore_grid(Agenda,D,RPath,[cost(C),depth(G)],Visited,Locations,Draft) :-
  achieved_explore(Locations,Draft).

achieved_explore(Locations,Draft):-
  length(Draft,10),
  Locations = Draft.

explore_grid(Agenda,D,RR,Cost,Visited,Locations,Draft) :-
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  Current = [c(F,G,Pos)|RPath],
  RPath = [Last|_],
  (map_adjacent(Last,OraclePos,o(O)),
    \+ memberchk((OraclePos, o(O)),Draft) ->
      NewDraft = [(OraclePos,o(O))|Draft]; NewDraft = Draft),
  ( setof([c(F1,G1,P1)|RR1], (search_astar(Pos,F1,G,G1,P1,RPath,RR1), \+ memberchk(P1,Visited)), Children)
  ->  append(Rest,Children ,NewAgenda), update_visited(Visited,Children, NewVisited) ; NewAgenda = Rest, NewVisited = Visited),
  D1 is D+1,
  explore_grid(NewAgenda,D1,RR,Cost,NewVisited,Locations,NewDraft).  % backtrack search

explore_grid(Agenda,D,RR,Cost,Visited,Locations,Draft) :-
  Locations = Draft.

% Predicate for finding the lowest cost (closest) charging/oracle location to go to,
% that has not been visited yet. This is done by generating a cost list, sorting it and
% choosing the one with the lowest cost that has not been visited yet.

find_lower_cost(Goal, P, Locations,Visited):-
  generate_cost_list(P,Locations,CostList,[]),
  sort(CostList,Sorted),
  Locations = [(_,ID)|_],
  (ID = o(O) -> find_lowest_unvisited(Sorted,Visited,NewGoal), NewGoal = (Cost,GoalPos,NewID) ;
   ID = c(C) -> Sorted = [(Cost,GoalPos,NewID)|_] ),
  Goal = (GoalPos, NewID).

% predicate for generating a cost list
generate_cost_list(P,Locations,CostList,Draft):-
  Locations = [],
  CostList = Draft.

generate_cost_list(P,Locations,CostList,Draft):-
  Locations = [Head|Tail],
  Head = (ChargingPos, ID),
  map_distance(P,ChargingPos,Cost),
  append(Draft,[(Cost,ChargingPos,ID)],NewDraft),
  generate_cost_list(P,Tail,CostList,NewDraft),!.

%Finds the lowest cost (closest) unvisited location
find_lowest_unvisited(Sorted,Visited,NewGoal):-
  Sorted = [(Cost,GoalPos,ID)|Tail],
  (\+ memberchk(ID, Visited) -> NewGoal =  (Cost,GoalPos,ID)
  ; find_lowest_unvisited(Tail, Visited, NewGoal)).

% Predicate for recursively trying to find oracles until the agent's identity is found.

%If one actor is left we are done.
find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
  ActorList = [[ actor(A)|_]].

%if 10 oracles were visited and still no identity was found just print this
find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
  length(VisitedOracles,10),
  writeln('Finished and I still do not know who I am').

% If identity still hasn't been found then check for the agent's energy. If it is less
% than a threshold then go to the closest charging location, else find the closest oracle.
% This is done by finding the closest path to the oracle/chargin station, parsing the path and
% discarding the last position in the path, which is the oracle/chargin station position. Thus the
% agent will move to the closest adjacent position for that oracle/charging station.

find_myself(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
  writeln('Iterating'),
  my_agent(Agent),
  query_world(agent_current_energy,[Agent,Energy]),
  query_world(agent_current_position,[Agent,P]),
  write('Energy'),writeln(Energy),
  ( Energy < 50 ->find_lower_cost(Goal,P,ChargingLocations,VisitedOracles),
                  Goal = (GoalPos,c(C)),
                  write("Charging Goal Pos "),writeln(GoalPos),
                  write("Charging Station "),writeln(C),
                  Task = go(GoalPos),
                  solve_task_astar_p3(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos,[P]),!,  % prune choice point for efficiency
                  reverse(R,[_Init|Path]),
                  query_world( agent_do_moves, [Agent,Path] ),
                  NewVisitedOracles = VisitedOracles
                  ;
                  find_lower_cost(Goal,P,OraclesLocations,VisitedOracles),
                  Goal = (GoalPos,o(O)),
                  write("Oracle Goal Pos "),writeln(GoalPos),
                  write("Oracle Station "),writeln(O),
                  Task = go(GoalPos),
                  solve_task_astar_p3(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos,[P]),!,  % prune choice point for efficiency
                  reverse(R,[_Init|Path]),
                  query_world( agent_do_moves, [Agent,Path] ),
                  NewVisitedOracles = [o(O)|VisitedOracles] ) ,

   (Goal = (GoalPos,c(C)) ->  writeln('toping up'), query_world(agent_topup_energy,[Agent,c(C)]),
                              writeln('toped up'), ActorsLeft = ActorList
   ;Goal = (GoalPos,o(O)) ->  write('Asking Oracle'), writeln(O), whosleft(ActorList,o(O),ActorsLeft),
                              write('Actors Left'),length(ActorsLeft,Le),writeln(Le)) ,
   find_myself(A,ActorsLeft,NewVisitedOracles,ChargingLocations,OraclesLocations),!.


% Predicate for filtering the list by asking a specific oracle.
whosleft(List,O,FilteredList):-
 my_agent(Agent),
 query_world(agent_ask_oracle,[Agent,O,link,L]),
 filter_actors(List,L,FilteredList).

% Predicate for writting the position on the terminal
write_pos(Pos):-
  Pos = p(X,Y),
  write(X), write(' '), writeln(Y).

%Modified predicate for use in BFs for finding oracle locations.
search_astar(Pos,F,G,G1,P1,RPath,NewRR) :-
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  F is G1,
  NewRR = [P1 | RPath].
