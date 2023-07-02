%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2023 2:42 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_utils).

-author("hasitha").

-include("kv_pb.hrl").

%% API
-export([decode_request_payload/1, encrypt_data/2, decrypt_data/1, store_data/2,
         retrieve_data/1, create_response/2, send_response/2]).

-spec decode_request_payload(binary()) -> ok | {error, pw_error:error_opts()}.
decode_request_payload(EncodedData) ->
    io:format("decode_request_payload|EncodedData:~p~n", [EncodedData]),
    kv_pb:decode_msg(EncodedData, req_envelope).

encrypt_data(KeyId, Data) ->
    io:format("encrypt_data|Data:~p~n", [Data]),
    io:format("encrypt_data|KeyId:~p~n", [KeyId]),
    ok = configure_aws(erlcloud_kms),
    case erlcloud_kms:encrypt(KeyId, base64:encode(Data)) of
        {ok, [{<<"CiphertextBlob">>, CiphertextBlob}, _, _]} ->
            io:format("encrypt_data|CiphertextBlob:~p~n", [CiphertextBlob]),
            {ok, CiphertextBlob};
        _ ->
            {error, internal}
    end.

decrypt_data(EncryptedData) ->
    io:format("decrypt_data|EncryptedData:~p~n", [EncryptedData]),
    ok = configure_aws(erlcloud_kms),
    case erlcloud_kms:decrypt(EncryptedData) of
        {ok, [_, _, _, {<<"Plaintext">>, Data}]} ->
            io:format("decrypt_data|Data:~p~n", [Data]),
            {ok, base64:decode(Data)};
        Other ->
            io:format("decrypt_data|Other:~p~n", [Other]),
            {error, internal}
    end.

store_data(Key, Data) ->
    ok = configure_aws(erlcloud_ddb2),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    case erlcloud_ddb2:put_item(TableName, [{<<"key">>, {s, Key}}, {<<"data">>, {s, Data}}])
    of
        {ok, _} ->
            ok;
        _ ->
            internal
    end.

retrieve_data(Key) ->
    ok = configure_aws(erlcloud_ddb2),
    io:format("~p|~p|retrieve_data|Key:~p~n", [?MODULE, ?LINE, Key]),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    io:format("~p|~p|retrieve_data|TableName:~p~n", [?MODULE, ?LINE, TableName]),
    case erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, Key}}) of
        {ok, []} ->
            {error, not_found};
        {ok, [{<<"key">>, _}, {<<"data">>, EncryptedData}]} ->
            io:format("retrieve_data|EncryptedData:~p~n", [EncryptedData]),
            {ok, EncryptedData};
        Other ->
            io:format("~p|~p|retrieve_data|Other:~p~n", [?MODULE, ?LINE, Other]),
            {error, internal}
    end.

create_response({get_request, {Key, Data}}, ok) ->
    #req_envelope{type = get_response_t,
                  get_resp = #get_response{req = #data{key = Key, value = Data}, error = ok}};
create_response({get_request, _}, Error) ->
    #req_envelope{type = get_response_t, get_resp = #get_response{error = Error}};
create_response({set_request, _}, Error) ->
    #req_envelope{type = set_response_t, set_resp = #set_response{error = Error}}.

send_response(Socket, Response) ->
    io:format("send_response|Response:~p~n", [Response]),
    io:format("send_response|Socket:~p~n", [Socket]),
    EncodedResponse = kv_pb:encode_msg(Response, req_envelope),
    io:format("send_response|EncodedResponse:~p~n", [EncodedResponse]),
    Result = gen_tcp:send(Socket, EncodedResponse),
    io:format("send_response|Result:~p~n", [Result]),
    ok.

configure_aws(Module) ->
    {ok, AccessKeyId} = application:get_env(miniclip_gpb, aws_access_key_id),
    io:format("configure_aws|AccessKeyId:~p~n", [AccessKeyId]),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    io:format("configure_aws|SecretAccessKey:~p~n", [SecretAccessKey]),
    Module:configure(AccessKeyId, SecretAccessKey),
    ok.
