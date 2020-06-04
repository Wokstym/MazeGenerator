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
-export([start_link/0, getMaze/0, handle_call/3, init/1, handle_cast/2, createEmptyMaze/2, step/0]).
-record(maze, {height, width, walls}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, no_maze_state, []).
init(Maze) ->
  {ok, Maze}.

%%%===================================================================
%%% API
%%%===================================================================

createEmptyMaze(Height, Width) -> gen_server:cast(?MODULE, {createEmptyMaze, Height, Width}).
getMaze() -> gen_server:call(?MODULE, getMaze).
step() -> gen_server:call(?MODULE, step).


%%%===================================================================
%%% Handlers
%%%===================================================================

handle_call(step, _From, no_maze_state) ->
  {reply, no_maze_state, no_maze_state};
handle_call(step, _From, MazeStructure) ->
  {Maze_finished_or_not_atom, {NewMaze, NewWalls}} = maze_generator:step(MazeStructure),
  {reply,  {Maze_finished_or_not_atom, NewMaze},  {Maze_finished_or_not_atom, {NewMaze, NewWalls}}};

handle_call(getMaze, _From, no_maze_state) ->
  {reply, no_maze_state, no_maze_state};
handle_call(getMaze, _From, {Maze_finished_or_not_atom, {Maze, Walls}}) ->
  {reply,  {Maze_finished_or_not_atom, Maze}, {Maze_finished_or_not_atom, {Maze, Walls}}}.



handle_cast({createEmptyMaze, Height, Width}, _OldMaze) ->
  MazeStructure = maze_generator:createMaze(Height, Width),
  {noreply, MazeStructure}.


