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

-record(maze, {height, width, passages, walls}).

countNeighbours()

validateAndInsert(Walls, X, Y, Height, Width) when X >= 0 and Y >= 0 and X < Width and Y < Height ->
  [{X, Y} | Walls];
validateAndInsert(Walls, _X, _Y, _Height, _Width) -> Walls.

insertAdjacentPositionsIfValid({Maze, WallsNotInMaze}, {X, Y}) ->
  WallsNotInMaze1 = validateAndInsert(WallsNotInMaze, X + 1, Y, Maze#maze.height, Maze#maze.width),
  WallsNotInMaze2 = validateAndInsert(WallsNotInMaze1, X, Y + 1, Maze#maze.height, Maze#maze.width),
  WallsNotInMaze3 = validateAndInsert(WallsNotInMaze2, X - 1, Y, Maze#maze.height, Maze#maze.width),
  validateAndInsert(WallsNotInMaze3, X, Y - 1, Maze#maze.height, Maze#maze.width).


createMaze(Height, Width) ->
  Maze = #maze{height = Height, width = Width, passages = #{}, walls=#{}},

  GenPassagePos = {rand:uniform(Width - 1), rand:uniform(Height - 1)},
  WallsNotInMaze = insertAdjacentPositionsIfValid({Maze, []}, GenPassagePos),

  {Maze, WallsNotInMaze}.

genNewPassage({Maze, []}) -> {finished_generation, {Maze, []}};
genNewPassage({Maze, WallsNotInMaze}) ->
  GenPassagePos = lists:nth(rand:uniform(length(WallsNotInMaze)), WallsNotInMaze),
  NewWalls = insertAdjacentPositionsIfValid({Maze, WallsNotInMaze}, GenPassagePos),





