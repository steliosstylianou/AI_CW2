
find_identity_4(A):-
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
  find_myself_4(A,ActorList,[],ChargingLocations,OraclesLocations).
  find_myself_4(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
    ActorList = [[ actor(A)|_]].

  find_myself_4(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
    length(VisitedOracles,10),
    writeln('Finished and I still do not know who I am').

  find_myself_4(A,ActorList,VisitedOracles,ChargingLocations,OraclesLocations):-
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
                    part4_move(Agent, Path),
                    NewVisitedOracles = VisitedOracles
                    ;
                    find_lower_cost(Goal,P,OraclesLocations,VisitedOracles),
                    Goal = (GoalPos,o(O)),
                    write("Oracle Goal Pos "),writeln(GoalPos),
                    write("Oracle Station "),writeln(O),
                    Task = go(GoalPos),
                    solve_task_astar_p3(Task,[[c(0,0,P),P]],0,R,Cost,_NewPos,[P]),!,  % prune choice point for efficiency
                    reverse(R,[_Init|Path]),
                    part4_move(Agent, Path),
                    NewVisitedOracles = [o(O)|VisitedOracles] ) ,

     (Goal = (GoalPos,c(C)) ->  writeln('toping up'), query_world(agent_topup_energy,[Agent,c(C)]),
                                writeln('toped up'), ActorsLeft = ActorList
     ;Goal = (GoalPos,o(O)) ->  write('Asking Oracle'), writeln(O), whosleft(ActorList,o(O),ActorsLeft),
                                write('Actors Left'),length(ActorsLeft,Le),writeln(Le)) ,
     find_myself_4(A,ActorsLeft,NewVisitedOracles,ChargingLocations,OraclesLocations),!.

part4_move(Agent, Path):-
  Path = [Head|Tail],
  query_world( agent_do_moves, [Agent,Head]),
  part4_move(Agent, Tail).

part4_move(Agent, Path):-
  Path = [Head|Tail],
  Tail = [HeadT|TailT],
  query_world(agent_current_position,[Agent,P]),
  (solve_task_astar(go(HeadT),[[c(0,0,P),P]],0,R,Cost,NewPos,[])->
    part4_move(Agent, R),
    part4_move(Agent,TailT)
    ;
    writeln("not yet implemented")).
