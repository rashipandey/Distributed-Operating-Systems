-module(fingertable).
-export([send_ft/2]).

fetch_ith_child(Hash, NCondition, I,  M) -> 
    case dict:find((Hash + I) rem trunc(math:pow(2, M)), NCondition) of
        error ->
             fetch_ith_child(Hash, NCondition, I + 1, M);
        _ -> (Hash + I) rem trunc(math:pow(2, M))
    end.

send_ft_nodes([], _, _) ->
    ok;
send_ft_nodes(NodesToSend, NCondition, FTs) ->
    [Start|Rest] = NodesToSend,
    Pid = dict:fetch(Start ,NCondition),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(Start, FTs))},
    send_ft_nodes(Rest, NCondition, FTs).

send_ft(NCondition,M) ->
    FTs = gather_fts(NCondition, dict:to_list(NCondition), dict:new(),M),
    io:format("~n~p~n", [FTs]),
    send_ft_nodes(dict:fetch_keys(FTs), NCondition, FTs).

fetch_ft(_, _, M, M,FingerList) ->
    FingerList;
fetch_ft(Node, NCondition, M, I, FingerList) ->
    Hash = element(1, Node),
    Ith_child = fetch_ith_child(Hash, NCondition, trunc(math:pow(2, I)), M),
    fetch_ft(Node, NCondition, M, I + 1, FingerList ++ [{Ith_child, dict:fetch(Ith_child, NCondition)}] ).

gather_fts(_, [], FTDict,_) ->
    FTDict;

gather_fts(NCondition, NetList, FTDict,M) ->
    [Start | Rest] = NetList,
    FTs = fetch_ft(Start, NCondition,M, 0,[]),
    gather_fts(NCondition, Rest, dict:store(element(1, Start), FTs, FTDict), M).



