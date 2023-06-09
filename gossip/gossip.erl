-module(gossip).
-export([spawn_actors/1, pass_message_to_neighbours / 0, index_of/2]).

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

spawn_actors(Counter)->
  receive {"gossipmessage", Index, NeighbourList, NodeList, Parent} ->
    io:format("Inside spawn actors receive and counter is ~n"),
    Parent,
    if
      Counter == 1 ->
        io:format("Convergence has been attained for ~p ~n", [self()]),
        convergence ! {"Actor Completed His Work", self()},
        pass_message_to_neighbours ! {"No longer able to acknowledge messages", "Dead", Parent, NodeList, NeighbourList, self()},
        exit(normal);
      true ->
        pass_message_to_neighbours ! {Index, NeighbourList, NodeList, self()},
        spawn_actors(Counter - 1)
    end
  end.

recursive(0,_,Count) ->
  Count;

recursive (Len , Neighbours, Count) ->
  Neighbour_Pid = lists:nth(Len, Neighbours),
  Is_Neighbor_Alive = is_process_alive(Neighbour_Pid),
    if Is_Neighbor_Alive ->
      recursive (Len -1,Neighbours,Count+1);
    true ->
      recursive (Len -1,Neighbours,Count)
    end.

pass_message_to_neighbours() ->
  receive
    {Index, NeighbourList, NodeList, PID} ->
      Neighbours = lists:nth(Index, NeighbourList),
      Len = length(Neighbours),

      Rand_index = rand:uniform(Len),
      Neighbour_Pid = lists:nth(Rand_index, Neighbours),
      Is_Neighbor_Alive = is_process_alive(Neighbour_Pid),
      Num_Neighbours_Alive = recursive(Len,Neighbours,0),
      if
        Num_Neighbours_Alive =< 0 ->
          self() ! {index_of(PID, NodeList), NeighbourList, NodeList, PID},
          pass_message_to_neighbours();
      true ->
        if
          Is_Neighbor_Alive == false ->
          self() ! {Index, NeighbourList, NodeList, PID},
          pass_message_to_neighbours();
        true ->
          Neighbour_Pid ! {"gossipmessage", index_of(Neighbour_Pid, NodeList), NeighbourList, NodeList, PID},
          self() ! {Index, NeighbourList, NodeList, PID},
          pass_message_to_neighbours()
        end
      end;

    {"No longer able to acknowledge messages", State , PID, NodeList, NeighbourList, Neighbour_Pid} ->
      State, Neighbour_Pid,
      self() ! {index_of(PID, NodeList), NeighbourList, NodeList, PID},
      pass_message_to_neighbours()
  end.

