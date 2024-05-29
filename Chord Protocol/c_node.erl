
-module(c_node).
-export([create_nodes/5, node_listen/1, node/4]).


fetch_forward_dist(Key, Key, _, Distance) ->
    Distance;

fetch_forward_dist(Key, NodeId, M, Distance) ->
    fetch_forward_dist(Key, (NodeId + 1) rem trunc(math:pow(2, M)), M, Distance + 1).

node_listen(NodeCondition) ->
    Hash = dict:fetch(id, NodeCondition),
    receive
        {lookup, Id, Key, CountOfHops, _Pid} ->

            NodeVal = fetch_nearest_node(Key, dict:fetch_keys(dict:fetch(finger_table ,NodeCondition)), NodeCondition),
            UpdatedCondition = NodeCondition,
            io:format("~n Lookup::: ~p  For Key ~p  ClosestNode ~p ~n", [Hash, Key, NodeVal]),
            if
                (Hash == Key) ->
                    taskcompletionmonitor ! {completed, Hash, CountOfHops, Key};
                (NodeVal == Key) and (Hash =/= Key) ->
                    taskcompletionmonitor ! {completed, Hash, CountOfHops, Key};
                true ->
                    dict:fetch(NodeVal, dict:fetch(finger_table, NodeCondition)) ! {lookup, Id, Key, CountOfHops + 1, self()}
            end
    ;
        {kill} ->
            UpdatedCondition = NodeCondition,
            exit("Exit Signal ~n");
        {state, Pid} -> Pid ! NodeCondition,
            UpdatedCondition = NodeCondition;
        {fetch_child, Id, Pid} ->
            FoundChild = child(Id, NodeCondition),
            UpdatedCondition = NodeCondition,
            {Pid} ! {get_successor_reply, FoundChild};


        {fix_fingers, FT} ->
            UpdatedCondition = dict:store(finger_table, FT, NodeCondition)
    end,
    node_listen(UpdatedCondition).



randomNode(Node_id, []) -> Node_id;
randomNode(_, LivingNodes) -> lists:nth(rand:uniform(length(LivingNodes)), LivingNodes).

fetch_nearest_node(Key, IdsOfFingerNode, Condition) ->
    case lists:member(Key, IdsOfFingerNode) of
        true -> Key;
        _ -> nearest_node(Key, IdsOfFingerNode, -1, 10000000, Condition)
    end.


nearest_node(_, [], LowestNode, _, _) ->
    LowestNode;
nearest_node(Key, IdsOfFingerNode, LowestNode, LowestVal, Condition) ->
    [Start| Rest] = IdsOfFingerNode,
    Distance = fetch_forward_dist(Key, Start, dict:fetch(m, Condition), 0),
    if
        Distance < LowestVal ->
            nearest_node(Key, Rest, Start, Distance, Condition);
        true -> 
            nearest_node(Key, Rest, LowestNode, LowestVal, Condition)
    end.

nearest_previous_finger(_, NodeCondition, 0) -> NodeCondition;
nearest_previous_finger(Id, NodeCondition, M) ->
    MthFinger = lists:nth(M, dict:fetch(finger_table, NodeCondition)),

    case in_range_check(dict:fetch(id, NodeCondition), Id, dict:fetch(node ,MthFinger), dict:fetch(m, NodeCondition)) of
        true ->

            dict:fetch(pid ,MthFinger) ! {state, self()},
            receive
                {statereply, FingerNodeState} ->
                    FingerNodeState
            end,
            FingerNodeState;
        _ -> nearest_previous_finger(Id, NodeCondition, M - 1)
    end
.

in_range_check(From, To, Key, M) ->
    if 
        From < To -> 
            (From =< Key) and (Key =< To);
        trunc(From) == trunc(To) ->
            trunc(Key) == trunc(From);
        From > To ->
            ((Key >= 0) and (Key =< To)) or ((Key >= From) and (Key < trunc(math:pow(2, M))))
    end.


create_nodes(CNodes, _, _, 0, NCondition) ->
    [CNodes, NCondition];
create_nodes(CNodes, TotalNodes, M, NumNodes, NCondition) ->
    [Hash, NewNetworkCondition] = add_node_chord(CNodes, TotalNodes,  M, NCondition),
    create_nodes(lists:append(CNodes, [Hash]), TotalNodes, M, NumNodes - 1, NewNetworkCondition).


parent(Id, NodeCondition) ->
    case 
        in_range_check(dict:fetch(id, NodeCondition) + 1, dict:fetch(id, dict:fetch(child, NodeCondition)), Id, dict:fetch(m, NodeCondition)) of 
            true -> NodeCondition;
            _ -> parent(Id, nearest_previous_finger(Id, NodeCondition, dict:fetch(m, NodeCondition)))
    end
.

child(Id, NodeCondition) ->
    PredicessorNodeState = parent(Id, NodeCondition),
    dict:fetch(child, PredicessorNodeState)
.


node(Hash, M, CNodes, _NodeState) ->
    FT = lists:duplicate(M, randomNode(Hash, CNodes)),
    UpdatedNodeCondition = dict:from_list([{id, Hash}, {parent, nil}, {finger_table, FT}, {next, 0}, {m, M}]),
    node_listen(UpdatedNodeCondition).


add_node_chord(CNodes, TotalNodes, M, NCondition) ->
    LeftHashes = lists:seq(0, TotalNodes - 1, 1) -- CNodes,
    Hash = lists:nth(rand:uniform(length(LeftHashes)), LeftHashes),
    Pid = spawn(c_node, node, [Hash, M, CNodes, dict:new()]),
    io:format("~n ~p ~p ~n", [Hash, Pid]),
    [Hash, dict:store(Hash, Pid, NCondition)]
.

