%%%-------------------------------------------------------------------
%%% @author grzeg
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. maj 2020 21:10
%%%-------------------------------------------------------------------
-module(maze_generator).
-author("grzeg").

%% API
-export([createMaze/2]).

-record(maze, {height, width, walls}).

createMaze(Height, Width) ->
  #maze{height = Height, width = Width, walls = #{}}.

