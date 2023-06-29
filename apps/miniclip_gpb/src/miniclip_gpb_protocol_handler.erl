%%%-------------------------------------------------------------------
%%% @author h.dhananjaya
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2023 1:31 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_protocol_handler).
-author("h.dhananjaya").

-behaviour(ranch_protocol).

%% API
-export([start_link/3]).
-export([init/3]).

start_link(Ref, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
  {ok, Pid}.

init(Ref, Transport, _Opts = []) ->
  {ok, Socket} = ranch:handshake(Ref),
  loop(Socket, Transport).

loop(Socket, Transport) ->
  case Transport:recv(Socket, 0, 60000) of
    {ok, Data} when Data =/= <<4>> ->
      Transport:send(Socket, Data),
      loop(Socket, Transport);
    _ ->
      ok = Transport:close(Socket)
  end.