-module(main).
-import(c_node, [create_nodes/5, node_listen/1, node/4]).
-import(messages, [send_msg_kill/5, listen_completion_of_task/2]).
-import(fingertable, [send_ft/2]).
-export([main/2, create_ntw/2]).

main(NumNodes, NumRequest) ->
    register(main_process, spawn(main, create_ntw, [NumNodes, NumRequest])).

create_ntw(NumNodes, NumRequest) ->
    M = fetch_m(NumNodes),
    [CNodes, NCondition] = create_nodes([], round(math:pow(2, M)), M, NumNodes, dict:new()),
    send_ft(NCondition,M),
    send_msg_kill(CNodes, NumNodes, NumRequest, M, NCondition).

fetch_m(NumNodes) ->
    trunc(math:ceil(math:log2(NumNodes))).
