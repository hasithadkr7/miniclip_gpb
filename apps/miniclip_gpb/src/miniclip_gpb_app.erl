%%%-------------------------------------------------------------------
%% @doc miniclip_gpb public API
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_gpb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _} = ranch:start_listener(tcp_gpb,
        ranch_tcp, #{socket_opts => [{port, 5555}]},
        miniclip_gpb_protocol_handler, []),
    miniclip_gpb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
