candidate_number(12345).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos,[P]),!,  % prune choice point for efficiency
  writeln('Found path'),
  reverse(R,[_Init|Path]),
  query_world( agent_do_moves, [Agent,Path] ),
  write('Moved').

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).

solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_]
  ).
achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

search(F,N,N,1) :-
  map_adjacent(F,N,empty).

  %%%%%%%%%% A-STAR SEARCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % agenda is a list of current structures

solve_task_astar(Task,Agenda,D,RPath,[cost(C),depth(G)],NewPos,Visited) :-
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  Current = [c(F,G,Pos)|RPath],
  achieved_v2(Task,Current,RPath,C,NewPos).

solve_task_astar(Task,Agenda,D,RR,Cost,NewPos,Visited) :-
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  Current = [c(F,G,Pos)|RPath],
  ( setof([c(F1,G1,P1)|RR1], (search_astar(Task,Pos,F1,G,G1,P1,RPath,RR1), \+ memberchk(P1,Visited)), Children)
  ->  append(Rest,Children ,NewAgenda), update_visited(Visited,Children, NewVisited) ; NewAgenda = Rest, NewVisited = Visited),
  D1 is D+1,
  solve_task_astar(Task,NewAgenda,D1,RR,Cost,NewPos,NewVisited).  % backtrack search

search_astar(go(P),Pos,F,G,G1,P1,RPath,NewRR) :-
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  map_distance(P1,P,H),
  F is G1 + H,
  NewRR = [P1 | RPath].

search_astar(find(O),Pos,F,G,G1,P1,RPath,NewRR) :-
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  F is G1,
  NewRR = [P1 | RPath].

achieved_v2(go(Exit),Current,RPath,Cost,NewPos) :-
  % writeln('trying Achieve'),
  Current = [c(Cost,G,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise ->  RPath = [Exit|_]
  ).

achieved_v2(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,G,NewPos)|_],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

print_agenda([]).
print_agenda(Agenda) :-
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  write('F = '), write(F), write(' G = '), write(G), write(' Pos = '), write(Pos),write(' RPath = '),write(RPath).
  print_agenda(Rest).

update_visited(Visited,[],Visited).

update_visited(Visited,Children,NewVisited):-
  Children = [[c(F,G,P)|RR]|RestChildren],
  update_visited([P|Visited], RestChildren, NewVisited).

% Part 3: BFS for finding OraclesLocations

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

search_astar(Pos,F,G,G1,P1,RPath,NewRR) :-
  % writeln('Searching astar'),
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  F is G1,
  NewRR = [P1 | RPath].
