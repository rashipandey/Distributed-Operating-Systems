-module(main).
-export([start/0]).
-export([setup_nodes_in_topology/3,  create_2d_topology/2, create_2d_topology/4, create_imperfect3d_topology/2, create_imperfect3d_topology/5 ,is_convergence_achieved/2]).
-define(GOSSIP_MESSAGE, "gossipmessage").

start() ->
  {_, [Actor]} = io:fread("Enter the number of Nodes: ", "~d"),
  {_, [Topology]} = io:fread("Enter the topology type: ", "~s"),
  {_, [Algorithm]} = io:fread("Add the algorithm that will be used.: ", "~s"),
  io:format("Actors ~w, Topology ~p, Algorithms ~p ~n", [Actor, Topology, Algorithm]),

  Convergence_Pid = spawn(?MODULE, is_convergence_achieved, [Actor, erlang:system_time(millisecond)]),
  register(convergence, Convergence_Pid),

  if
    Algorithm  == "gossip"->
      io:fwrite("Beginning Gossip Algorithm ~n"),
      ActorList = [spawn(gossip, spawn_actors, [10]) || _ <- lists:seq(1, Actor)];

    Algorithm == "pushsum" ->
      io:fwrite("Beginning Push Sum Algorithm ~n"),
      ActorList = [spawn(push_sum, spawn_actors, [Idx, 1, 0]) || Idx <- lists:seq(1, Actor)];

    true -> ActorList =[]
  end,

  NeighbourList = setup_nodes_in_topology(Actor, Topology, ActorList),
  Index = rand:uniform(Actor),
  if
    Algorithm  == "gossip"->
      Initial_Gossip_Pid = lists:nth(Index, ActorList),
      Neighbour_PID = spawn(gossip, pass_message_to_neighbours, []),
      register(pass_message_to_neighbours, Neighbour_PID),
      Initial_Gossip_Pid ! {"gossipmessage", Index, NeighbourList, ActorList, self()};

    Algorithm == "pushsum" ->
      Initial_Push_Sum_Pid = lists:nth(Index, ActorList),
      Neighbour_PID = spawn(push_sum, pass_message_to_neighbours, []),
      register(pass_message_to_neighbours, Neighbour_PID),
      Initial_Push_Sum_Pid ! {"Push_Sum", Index, NeighbourList, ActorList, self(), 0, 0};

    true -> do_nothing
  end,

  io:format("Process Initiated").

is_convergence_achieved(0, Start_Time) ->
  End_Time = erlang:system_time(millisecond),
  io:format("Start Time ~w", [Start_Time]),
  io:format("End Time ~w", [End_Time]),
  io:format("Convergence time is ~w milliseconds  ~n", [End_Time - Start_Time]),
  %%Shut the server and all the registered process.
  exit(whereis(pass_message_to_neighbours), ok),
  exit(whereis(convergence), ok),
  exit(self(), ok);

is_convergence_achieved(Actor, Start_Time) ->
  receive {"Actor Completed His Work", Pid} ->
    Pid,
    is_convergence_achieved(Actor - 1, Start_Time)
  end.

setup_nodes_in_topology(Actors, Topology, ActorList) ->
  Neighbour_List = create_neighbours(ActorList, Topology, Actors),
  io:format("Actor list is ~p ~n and neighbor list is ~p ~n", [ActorList, Neighbour_List]),
  Neighbour_List.

create_neighbours(ActorList, Topology, Actors) ->
  io:format("creating neighbours for actors ~p  and Topology is ~p ~n", [Actors, Topology]),
  if
    Topology == "full" ->
      Neighbours = create_full_network_topology(Actors, ActorList);

    Topology == "line" ->
      Neighbours = create_line_topology(Actors, ActorList);

    Topology == "2d" ->
      Neighbours = create_2d_topology(Actors, ActorList);

    Topology == "imperfect3d" ->
      Neighbours = create_imperfect3d_topology(Actors, ActorList);
    true -> invalid_topology, Neighbours = []
  end,
  Neighbours.

%%%% Construct Neighbours for all topologies

%%% Full Network Topology Implementation
create_full_network_topology(Actors, ActorList) ->
  create_full_network_topology(Actors, ActorList, []).

create_full_network_topology(0, ActorList, Neighbours) ->
  ActorList,
  lists:reverse(Neighbours);

create_full_network_topology(Index, ActorList, Neighbours) ->
  ActorNeighbours = ActorList -- [lists:nth(Index, ActorList)],
  create_full_network_topology(Index - 1, ActorList, lists:append(Neighbours, [ActorNeighbours])).

%%% Line Topology Implementation
create_line_topology(Actors, ActorList) ->
  create_line_topology(Actors, Actors, ActorList, []).

create_line_topology(0, Actors, ActorList, Neighbours) ->
  ActorList, Actors,
  lists:reverse(Neighbours);

create_line_topology(Index, Actors, ActorList, Neighbours) ->
  if
    Index == 1 ->
      Next_Ele = lists:nth(Index + 1, ActorList),
      create_line_topology(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Next_Ele]]));

    Index == Actors ->
      Prev_Ele = lists:nth(Index - 1, ActorList),
      create_line_topology(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Prev_Ele]]));
    
    true ->
      Next_Ele = lists:nth(Index + 1, ActorList),
      Prev_Ele = lists:nth(Index - 1, ActorList),
      create_line_topology(Index - 1, Actors, ActorList, lists:append(Neighbours, [[Prev_Ele, Next_Ele]]))
  end.

%%% 2D Topology Implementation
populate_grid(Idx, Rows, Actors, ActorList, RowEle, Matrix) ->
  if
    Idx > Actors -> Matrix;
    true ->
      Ele = lists:nth(Idx, ActorList),
      TempRow = lists:append(RowEle, [Ele]),
      if
        Idx rem Rows == 0 ->
          populate_grid(Idx + 1, Rows, Actors, ActorList, [], lists:append(Matrix, [TempRow]));
        true ->
          populate_grid(Idx + 1, Rows, Actors, ActorList, TempRow, Matrix)
      end
  end.

%%% 2D Topology Implementation
create_2d_topology(Actors , ActorList) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  populate_grid(1, Rows, Actors, ActorList, [], []),
  create_2d_topology(Actors, Grid_Matrix, Rows, []).

create_2d_topology(0, Grid_Matrix, Rows, Neighbours) ->
  Grid_Matrix, Rows,
  lists:reverse(Neighbours);

create_2d_topology(Index, Grid_Matrix, Rows, Neighbours) ->
  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Ele_col = Rows,
      Ele_rows  = round(Index / Rows);
    true ->
      Ele_col = Index rem Rows,
      Ele_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  if
    Ele_rows == 1 ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows , Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        true ->
          N1 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]))
      end;
    Ele_rows == Rows ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2]]));
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]))
      end;
    true ->
      if
        Ele_col == 1 ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]));
        Ele_col == Rows ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3]]));
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N4 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          create_2d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3, N4]]))
      end
  end.

%%% Imperfect 3D Topology
create_imperfect3d_topology(Actors , ActorList) ->
  Rows =  round(math:sqrt(Actors)),
  Grid_Matrix =  populate_grid(1, Rows, Actors, ActorList, [], []),
  create_imperfect3d_topology(Actors, Grid_Matrix, Rows, [], ActorList).

create_imperfect3d_topology(0, Grid_Matrix, Rows, Neighbours, ActorList) ->
  Grid_Matrix, Rows, ActorList,
  lists:reverse(Neighbours);

create_imperfect3d_topology(Index, Grid_Matrix, Rows, Neighbours, ActorList) ->
  Grid_Matrix, Neighbours,
  if
    Index rem Rows == 0 ->
      Ele_col = Rows,
      Ele_rows  = round(Index / Rows);
    true ->
      Ele_col = Index rem Rows,
      Ele_rows = trunc(math:floor((Index / Rows))) + 1
  end,
  Ele =  lists:nth(Ele_col, lists:nth(Ele_rows, Grid_Matrix)),
  if
    Ele_rows == 1 ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2,lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ActorList);
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows , Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ActorList);
        true ->
          N1 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col, lists:nth(Ele_rows + 1, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, N3, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ActorList)
      end;
    Ele_rows == Rows ->
      if
        Ele_col == 1 ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ActorList);
        Ele_col == Rows ->
          N1= lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ActorList);
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, N3, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3, lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ActorList)
      end;
    true ->
      if
        Ele_col == 1 ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, N3, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List) ]]), ActorList);
        Ele_col == Rows ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, N3, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ActorList);
        true ->
          N1 = lists:nth(Ele_col, lists:nth(Ele_rows - 1, Grid_Matrix)),
          N2= lists:nth(Ele_col , lists:nth(Ele_rows + 1, Grid_Matrix)),
          N3 = lists:nth(Ele_col - 1, lists:nth(Ele_rows, Grid_Matrix)),
          N4 = lists:nth(Ele_col + 1, lists:nth(Ele_rows, Grid_Matrix)),
          Rem_List = ActorList -- [N1, N2, N3, N4, Ele],
          create_imperfect3d_topology(Index - 1, Grid_Matrix, Rows, lists:append(Neighbours, [[N1, N2, N3, N4,  lists:nth( rand:uniform(length(Rem_List)), Rem_List)]]), ActorList)
      end
  end.







