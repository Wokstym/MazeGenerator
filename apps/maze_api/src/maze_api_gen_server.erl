%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. maj 2020 21:39
%%%-------------------------------------------------------------------
-module(maze_api_gen_server).
-behaviour(gen_server).
-author("grzeg").

%% API
-export([start_link/0, getMonitor/0, handle_call/3, init/1, handle_cast/2, createEmptyMaze/2]).
-record(maze, {height, width, walls}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, "No maze created", []).
init(Maze) ->
  {ok, Maze}.

createEmptyMaze(Height, Width) -> gen_server:cast(?MODULE, {setEmptyMaze, Height, Width}).
getMonitor() -> gen_server:call(?MODULE, getVal).




handle_call(getVal, _From, Val) ->
  erlang:display(Val),
  {reply, Val, Val}.


handle_cast({setEmptyMaze, Height, Width}, _OldMaze) ->
  XD= maze_generator:createMaze(Height, Width),
  XD2 = XD#maze{walls = maps:put({1,1}, true, XD#maze.walls)},
  {noreply,XD2}.

