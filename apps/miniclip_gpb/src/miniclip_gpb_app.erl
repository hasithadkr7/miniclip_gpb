%%%-------------------------------------------------------------------
%% @doc miniclip_gpb public API
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_gpb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    miniclip_gpb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
