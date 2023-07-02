%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Jun 2023 4:39 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_client_test).

-author("hasitha").

%% API
-export([set_data/2, get_data/1]).

get_data(Key) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}]),
    Envelop = miniclip_gpb_proto_test:encode_get_request_envelope(Key),

    ok = gen_tcp:send(Sock, Envelop).

%%    ok = gen_tcp:close(Sock).

set_data(Key, Value) ->
    {ok, Sock} = gen_tcp:connect("localhost", 5555, [binary, {packet, 0}]),
    Envelop = miniclip_gpb_proto_test:encode_set_request_envelope(Key, Value),

    ok = gen_tcp:send(Sock, Envelop).%%    ok = gen_tcp:close(Sock).
