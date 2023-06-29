-module(worker).
-export([start/1]).

start(Master)->
    net_adm:ping(Master),
    {server, Master} ! {node, node()}.