-module(messages).
-export([send_msg_kill/5, listen_completion_of_task/2]).

send_msg_node(_, [], _) ->
    ok;
send_msg_node(Key, CNodes, NCondition) ->
    [Start | Rest] = CNodes,
    Pid = fetch_node_pid(Start, NCondition),
    Pid ! {lookup, Start, Key, 0, self()},
    send_msg_node(Key, Rest, NCondition)
.

terminate_nodes([], _) ->
    ok;
terminate_nodes(CNodes, NCondition) ->
    [Start | Rest] = CNodes,
    fetch_node_pid(Start, NCondition) ! {kill},
    terminate_nodes(Rest, NCondition).


fetch_hops_total() ->
    receive
        {totalhops, CountOfHops} ->
            CountOfHops
    end.
fetch_node_pid(Hash, NCondition) ->
    case dict:find(Hash, NCondition) of
        error -> nil;
        _ -> dict:fetch(Hash, NCondition)
    end
.

send_msgs_nodes(_, 0, _, _) ->
    ok;
send_msgs_nodes(CNodes, NumRequest, M, NCondition) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(CNodes)), CNodes),
    send_msg_node(Key, CNodes, NCondition),
    send_msgs_nodes(CNodes, NumRequest - 1, M, NCondition)
.

send_msg_kill(CNodes, NumNodes, NumRequest, M, NCondition) ->
    register(taskcompletionmonitor, spawn(messages, listen_completion_of_task, [NumNodes * NumRequest, 0])),
    send_msgs_nodes(CNodes, NumRequest, M, NCondition),
    HopsTotal = fetch_hops_total(),
    {ok, File} = file:open("./chordp2pstats.txt", [append]),
    io:format("~n Average Hops : ~p, Total Hops : ~p  for ~p Num of Nodes and ~p Num of Requests  ~n", [HopsTotal/(NumNodes * NumRequest), HopsTotal, NumNodes , NumRequest]),
    terminate_nodes(CNodes, NCondition)
.

listen_completion_of_task(0, CountOfHops) ->
    main_process ! {totalhops, CountOfHops}
;

listen_completion_of_task(NumRequests, CountOfHops) ->
    receive
        {completed, _Pid, TaskHopsCount, _Key} ->
            io:format("~n ~p Completed , Number of Hops : ~p, Key : ~p ~n", [_Pid, TaskHopsCount, _Key]),
            listen_completion_of_task(NumRequests - 1, CountOfHops + TaskHopsCount)
    end
.