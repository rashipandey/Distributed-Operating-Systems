-module(push_sum).
-export([spawn_actors/3, pass_message_to_neighbours / 0, index_of/2]).
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

spawn_actors(S, W, Counter)->
    receive {"Push_Sum", Index, NeighbourList, NodeList, Sender, Received_S, Received_W} ->
        if
           Counter > 3 ->
               io:fwrite("Convergence Attained for ~p ~n", [self()]),
               convergence ! {"Actor Finished Work", self()},
               pass_message_to_neighbours ! {"No longer able to acknowledge messages", "Dead", Sender, NodeList, NeighbourList, self(), Received_S, Received_W},
               exit(normal);
            true ->
                Original_Ratio = S / W,
                io:fwrite("old ratio is ~p ~n",[Original_Ratio]),
                Updated_S = S + Received_S,
                Updated_W = W + Received_W,
                Calculated_New_Ratio = Updated_S / Updated_W,
                io:fwrite("new ratio is ~p ~n",[Calculated_New_Ratio]),

                Ratio_Diff = abs(Calculated_New_Ratio - Original_Ratio),
                Threshold = math:pow(10, -10),
                if
                    Ratio_Diff < Threshold ->
                        NewCounter = Counter + 1;
                    true -> NewCounter = 0
                end,

                pass_message_to_neighbours ! {Index, NeighbourList, NodeList, self(), Updated_S / 2, Updated_W / 2},
                spawn_actors(Updated_S / 2, Updated_W / 2, NewCounter)
        end
    end.

pass_message_to_neighbours() ->
    receive
        {Index, NeighbourList, NodeList, PID, S, W} ->
            Neighbours = lists:nth(Index, NeighbourList),
            Len = length(Neighbours),
            Rand_index = rand:uniform(Len),
            Neighbour_Pid = lists:nth(Rand_index, Neighbours),
            Is_Neighbor_Alive = is_process_alive(Neighbour_Pid),
            if
                Is_Neighbor_Alive == false ->
                    self() ! {Index, NeighbourList, NodeList, PID, S, W},
                    pass_message_to_neighbours();
                true ->
                    Neighbour_Pid ! {"Push_Sum", index_of(Neighbour_Pid, NodeList), NeighbourList, NodeList, PID, S, W},
                    self() ! {Index, NeighbourList, NodeList, PID, S, W},
                    pass_message_to_neighbours()
            end;

        {"No longer able to acknowledge messages", State , PID, NodeList, NeighbourList, Neighbour_Pid, S, W} ->
            State, Neighbour_Pid,
            io:fwrite("Message from ~p to ~p ~n", [PID, Neighbour_Pid]),
            self() ! {index_of(PID, NodeList), NeighbourList, NodeList, PID, S, W},
            pass_message_to_neighbours()
    end.

