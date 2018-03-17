candidate_number(12345).

solve_task(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  % solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]). %TODO: REMOVE DOT
  % query_world( agent_do_moves, [Agent,Path] ).


solve_task2(Task,Cost):-
  my_agent(Agent),
  query_world( agent_current_position, [Agent,P] ),
  solve_task_bt(Task,[c(0,P),P],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  % solve_task_astar(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos),!,  % prune choice point for efficiency
  reverse(R,[_Init|Path]). %TODO: REMOVE DOT
  % query_world( agent_do_moves, [Agent,Path] ).

  %%%%%%%%%% A-STAR SEARCH %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % agenda is a list of current structures

solve_task_astar(Task,Agenda,D,RR,[cost(Cost),depth(Depth)],NewPos) :-
  %Parsing agenda to get current
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  RPath = [ Path | X ],
  Current = [c(F,G,Pos)],

  achieved_v2(Task,Current,Path,Cost,NewPos).

solve_task_astar(Task,Agenda,D,RR,Cost,NewPos) :-
  writeln('Solve astar'),
  Agenda =  [[c(F,G,Pos)|RPath]|Rest],
  Current = [c(F,G,Pos)|RPath],
  % print_fgp(F,G,Pos),
  ( setof([c(F1,G1,P1),RR1], search_astar(Task,Pos,F1,G,G1,P1,RPath,RR1), Children)
  -> append(Children,Agenda,NewAgenda) ; NewAgenda = Agenda),
  D1 is D+1,
  solve_task_astar(Task,NewAgenda,D1,RR,F1,NewPos).  % backtrack search

search_astar(go(P),Pos,F,G,G1,P1,RPath,NewRR) :-
  writeln('Searching astar'),
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  map_distance(P1,P,H),
  F is G1 + H,
  NewRR = [P1 | RPath].

search_astar(find(O),Pos,F,G,P1,R,RPath) :-
  writeln('Searching astar'),
  map_adjacent(Pos,P1,empty),
  \+ memberchk(P1, RPath),  % check we have not been here already
  G1 is G+1,
  F is G1,
  NewRR = [P1 | RPath].

achieved_v2(go(Exit),Current,RPath,Cost,NewPos) :-
  writeln('trying Achieve'),
  
  Current = [c(Cost,G,NewPos)|RR],
  ( Exit=none -> true
  ; otherwise ->  RPath = [Exit|_], writeln('achieved done!')
  ).

achieved_v2(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,G,Pos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*

solve_task_bt(Task,Current,Depth,RPath,[cost(Cost),depth(Depth)],NewPos) :-
  achieved(Task,Current,RPath,Cost,NewPos).

solve_task_bt(Task,Current,D,RR,Cost,NewPos) :-
  writeln('solve bt'),
  Current = [c(F,P)|RPath],
  search(P,P1,R,C),
  \+ memberchk(R,RPath),  % check we have not been here already
  D1 is D+1,
  F1 is F+C,
  solve_task_bt(Task,[c(F1,P1),R|RPath],D1,RR,Cost,NewPos).  % backtrack search

achieved(go(Exit),Current,RPath,Cost,NewPos) :-
  writeln('trying Achieve'),
  Current = [c(Cost,NewPos)|RPath],
  ( Exit=none -> true
  ; otherwise -> RPath = [Exit|_], writeln('Achieve false')
  ).

achieved(find(O),Current,RPath,Cost,NewPos) :-
  Current = [c(Cost,NewPos)|RPath],
  ( O=none    -> true
  ; otherwise -> RPath = [Last|_],map_adjacent(Last,_,O)
  ).

% F = current pos
search(F,N,N,1) :-
  map_adjacent(F,N,empty).
