%%%-------------------------------------------------------------------
%% @doc maze_api top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(maze_api_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
      #{id=> 'maze_srv',
        start=> {maze_api_gen_server, start_link, []},
        restart=> permanent,
        shutdown=>2000,
        type => worker,
        modules => [maze_api_gen_server]
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
