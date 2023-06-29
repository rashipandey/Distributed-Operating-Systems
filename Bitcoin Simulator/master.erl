-module(master).

-compile(export_all).

start(K , Num_Workers) ->
    Leading_Zeroes = lists:concat(lists:duplicate(K, "0")),
    io:fwrite("Input number of 0s : ~p\n", [K]),
    io:fwrite("Input number of worker nodes : ~p\n", [Num_Workers]),
    spawn_workers(Num_Workers, Leading_Zeroes, node(), self()),
    register(server, self()),
    statistics(runtime),
    {Clock_Time, _} = timer:tc(master, master, [0, Num_Workers, Leading_Zeroes]),
    {_, CPU_Time} = statistics(runtime),
    io:fwrite("Total clock time: ~p\nTotal CPU time: ~p\n Ratio of real time vs CPU time: ~p\n", [Clock_Time/1000, CPU_Time, CPU_Time/(Clock_Time/1000)]).

spawn_workers(0, _, _, _) ->
    ok;
spawn_workers(N, Leading_Zeroes, Slave, Master) ->
    spawn(Slave, master, begin_mining, [Leading_Zeroes, Master]),
    spawn_workers(N - 1, Leading_Zeroes, Slave, Master).

master(100, _, _) ->
    ok;
master(Coins_found, Num_Workers, Leading_Zeroes) ->
    receive
        {Hash, Random_Gen_Str, Finder} ->
            io:fwrite("Coin: ~p\nHash: ~p\nFinder ID: ~p\n\n", [Random_Gen_Str, Hash, Finder]),
            Finder ! {mine},
            master(Coins_found+1, Num_Workers, Leading_Zeroes);
        {node, Slave} ->
            io:fwrite("Spawning workers on client ~p\n", [Slave]),
            send_code(Slave),
            spawn(master, spawn_workers, [Num_Workers, Leading_Zeroes, Slave, self()]),
            master(Coins_found, Num_Workers, Leading_Zeroes)
    end.

begin_mining(Leading_Zeroes, Master) ->
    Str = base64:encode(crypto:strong_rand_bytes(72)),
    Gen_Str=binary_to_list(Str),
    Random_Gen_Str = string:join(["preranavenugopal", Gen_Str], ";"),
    Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, Random_Gen_Str))]),
    BitCoin =  string:find(Hash, Leading_Zeroes) =:= Hash,
    if BitCoin == true ->
        Master ! {Hash, Random_Gen_Str, self()},
        receive
            {mine} ->
                begin_mining(Leading_Zeroes, Master)
        after
            10000 ->
                ok
        end;
        true ->
            begin_mining(Leading_Zeroes, Master)
    end.

send_code(Slave) ->
    {Mod, Bin, File} = code:get_object_code(master),
    rpc:multicall([Slave], code, load_binary, [Mod, File, Bin]),
    ok.