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
-export([createMaze/2, step/1]).

-record(maze, {height, width, passages}).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Creates Empty maze with provided size
%%
%% @spec createMaze(Height, Width) ->  {Maze, WallsNotInMaze}.
%% @end
%%--------------------------------------------------------------------
createMaze(Height, Width) ->
  GenPassagePos = {rand:uniform(Width - 1), rand:uniform(Height - 1)},
  Maze = #maze{height = Height, width = Width, passages = #{GenPassagePos => true}},
  WallsNotInMaze = insertAdjacentPositionsIfValid(Maze#maze.height, Maze#maze.width, [], GenPassagePos),
  {maze_not_finished_generating, {Maze, WallsNotInMaze}}.


%%--------------------------------------------------------------------
%% @doc
%% Does a Randomized Prim's algorithm step
%% described here: https://en.wikipedia.org/wiki/Maze_generation_algorithm#Randomized_Prim's_algorithm
%%
%% @spec step({maze_finished_or_not_atom, {Maze, WallsNotInMaze}}) ->  {Maze, WallsNotInMaze}.
%% @end
%%--------------------------------------------------------------------
step({_maze_finished_or_not_atom, {Maze, []}}) ->
  {finished_generation, {Maze, []}};
step({maze_not_finished_generating, {Maze, WallsNotInMaze}}) ->
  GenPassagePos = lists:nth(rand:uniform(length(WallsNotInMaze)), WallsNotInMaze),

  ReturnStruct = case returnPositionIfOneNeighbour(Maze#maze.passages, GenPassagePos) of
    more_than_one_neighbouring_passage ->
      {Maze, lists:delete(GenPassagePos, WallsNotInMaze)};
    {badmap, Passages} ->
      erlang:display("Passages should be map, got " + Passages),
      {Maze, lists:delete(GenPassagePos, WallsNotInMaze)};
    {NeighUniqPosX, NeighUniqPosY} ->
      %% Calculate position that passage lead to %%
      CellWhereNewPassageLeads = {
        2 * element(1, GenPassagePos) - NeighUniqPosX,
        2 * element(2, GenPassagePos) - NeighUniqPosY
      },
      ReturnMaze = Maze#maze{passages = maps:merge(
        Maze#maze.passages,
        #{GenPassagePos => true, CellWhereNewPassageLeads => true}
      )},
      WallsWithAdjacent = insertAdjacentPositionsIfValid(Maze#maze.height, Maze#maze.width, WallsNotInMaze, CellWhereNewPassageLeads),
      {ReturnMaze, lists:delete(GenPassagePos, WallsWithAdjacent)}
  end,
  {maze_not_finished_generating, ReturnStruct }.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts the server
%%
%% @spec mapIfPositionIsInMap(Passages, Position) -> Position |
%%                                                   false |
%%                                                   {badmap, Map}
%% @end
%%--------------------------------------------------------------------
mapIfPositionIsInMap(Passages, Position) ->
  case maps:is_key(Position, Passages) of
    true -> Position;
    Res -> Res
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% We check list of results provided from mapIfPositionIsInMap funtion and we check
%%
%% @spec returnPositionIfOne(ListOfResults, Position) -> more_than_one_neighbouring_passage |
%%                                                           {badmap, Passages} |
%%                                                           PositionTuple
%% @end
%%--------------------------------------------------------------------
returnPositionIfOne([{badmap, Passages} | _T], _) -> {badmap, Passages};
returnPositionIfOne([], Position) when is_tuple(Position) -> Position;
returnPositionIfOne([H | T], Position) when is_tuple(H) and is_tuple(Position) ->
  more_than_one_neighbouring_passage;
returnPositionIfOne([H | T], position_not_found_yet) when is_tuple(H) ->
  returnPositionIfOne(T, H);
returnPositionIfOne([_H | T], Passed) ->
  returnPositionIfOne(T, Passed).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If position provided has one neighbour it return position of that neighbour, otherwise
%% informing atom
%%
%% @spec returnPositionIfOneNeighbour(Passages, Position) -> more_than_one_neighbouring_passage |
%%                                                           {badmap, Passages} |
%%                                                           PositionTuple
%% @end
%%--------------------------------------------------------------------
returnPositionIfOneNeighbour(Passages, Position) ->
  ListOfResults = forEveryPosCallMapIfPosInMap(Passages, generateAdjacentPositions(Position)),
  returnPositionIfOne(ListOfResults, position_not_found_yet).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper method that calls for every provided position calls mapIfPositionIsInMap
%% and accumulates result in list
%%
%% @spec forEveryPosCallMapIfPosInMap(Passages, Positions) -> ListOfResults
%% @end
%%--------------------------------------------------------------------
forEveryPosCallMapIfPosInMap(_Passages, []) -> [];
forEveryPosCallMapIfPosInMap(Passages, [PosH | PosT]) ->
  [mapIfPositionIsInMap(Passages, PosH) | forEveryPosCallMapIfPosInMap(Passages, PosT)].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If position is within grid inserts it to provided list
%%
%% @spec ifValidInsert(Walls, Position, Height, Width) -> NewWalls
%% @end
%%--------------------------------------------------------------------
ifValidInsert(Walls, {X, Y}, Height, Width) when X >= 0, Y >= 0, X < Width, Y < Height ->
  [{X, Y} | Walls];
ifValidInsert(Walls, {_X, _Y}, _Height, _Width) -> Walls.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% For for adjacent positions to provided position calls validateAndInsert,
%% saves and returns result
%%
%% @spec insertAdjacentPositionsIfValid(Maze, WallsNotInMaze, PositionTuple) -> UpdatedWallsNotInMaze
%% @end
%%--------------------------------------------------------------------
insertAdjacentPositionsIfValid(Height, Width, WallsNotInMaze, Position) ->
  forEveryPosCallCalAndIns(Height, Width, WallsNotInMaze, generateAdjacentPositions(Position)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Helper method that calls for every provided position validateAndInsert function
%%
%% @spec forEveryPosCallCalAndIns(Height, Width, WallsNotInMaze, NeighbouringPositions) ->
%%                                              UpdatedWallsNotInMaze
%% @end
%%--------------------------------------------------------------------
forEveryPosCallCalAndIns(_Height, _Width, WallsNotInMaze, []) ->
  WallsNotInMaze;
forEveryPosCallCalAndIns(Height, Width, WallsNotInMaze, [NeighbouringH | NeighbouringT]) ->
  NewNotInMaze = ifValidInsert(WallsNotInMaze, NeighbouringH, Height, Width),
  forEveryPosCallCalAndIns(Height, Width, NewNotInMaze, NeighbouringT).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates list of positions adjacent to provided position
%%
%% @spec insertAdjacentPositionsIfValid(Maze, WallsNotInMaze, PositionTuple) -> UpdatedWallsNotInMaze
%% @end
%%--------------------------------------------------------------------
generateAdjacentPositions({X, Y}) ->
  [
    {X + 1, Y},
    {X - 1, Y},
    {X, Y + 1},
    {X, Y - 1}
  ].







