%%%-------------------------------------------------------------------
%%% @author hasitha
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

-spec start_link(reference(), atom(), list()) -> {ok, pid()}.
start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

-spec init(reference(), atom(), list()) -> ok.
init(Ref, Transport, _Opts = []) ->
    {ok, Socket} = ranch:handshake(Ref),
    loop(Socket, Transport).

-spec loop(socket:t(), atom()) -> ok.
loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 60000) of
        {ok, Data} when Data =/= <<4>> ->
            %%      Transport:send(Socket, Data),
            case miniclip_gpb_worker_fsm_sup:start_child([Transport, Socket]) of
                {ok, Pid} ->
                    gen_statem:cast(Pid, miniclip_gpb_utils:decode_request_payload(Data));
                {error, Reason} ->
                    io:format("worker start error|Reason:~p~n", [Reason])
            end,
            loop(Socket, Transport);
        _ ->
            ok = Transport:close(Socket)
    end.
