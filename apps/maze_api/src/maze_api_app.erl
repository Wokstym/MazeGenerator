%%%-------------------------------------------------------------------
%% @doc maze_api public API
%% @end
%%%-------------------------------------------------------------------

-module(maze_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    maze_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
