%%%-------------------------------------------------------------------
%% @doc miniclip_gpb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_gpb_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs = lists:flatten([miniclip_gpb_socket_server_sup:child_spec()]),
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
