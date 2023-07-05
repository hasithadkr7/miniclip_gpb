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

-define(IV, <<0:128>>).

%% API
-export([decode_request_payload/1, encrypt_data/2, decrypt_data/1, store_data/3,
         retrieve_data/1, create_response/2, send_response/2, configure_aws/1, get_data_key/0,
         encrypt_data/1]).

-spec decode_request_payload(binary()) -> ok | {error, pw_error:error_opts()}.
decode_request_payload(EncodedData) ->
    io:format("decode_request_payload|EncodedData:~p~n", [EncodedData]),
    kv_pb:decode_msg(EncodedData, req_envelope).

%%encrypt_data(KeyId, Data)->
%%    io:format("encrypt_data|Data:~p~n", [Data]),
%%    io:format("encrypt_data|KeyId:~p~n", [KeyId]),
%%    ok = configure_aws(erlcloud_kms),
%%    case erlcloud_kms:encrypt(KeyId, base64:encode(Data)) of
%%        {ok, [{<<"CiphertextBlob">>, CiphertextBlob}, _, _]} ->
%%            io:format("encrypt_data|CiphertextBlob:~p~n", [CiphertextBlob]),
%%            {ok, CiphertextBlob};
%%        Other ->
%%            io:format("encrypt_data|Other:~p~n", [Other]),
%%            {error, internal}
%%    end.

encrypt_data(Data) ->
    io:format("~p|~p|encrypt_data|Data:~p~n", [?MODULE, ?LINE, Data]),
    case get_data_key() of
        {ok, {EncryptedKey, Key}} ->
            io:format("~p|~p|encrypt_data|EncryptedKey:~p~n", [?MODULE, ?LINE, EncryptedKey]),
            io:format("~p|~p|encrypt_data|Key:~p~n", [?MODULE, ?LINE, Key]),
            case encrypt_data(Key, Data) of
                {ok, EncryptedData} ->
                    {ok, {EncryptedKey, base64:encode(EncryptedData)}};
                _ ->
                    {error, internal}
            end;
        Other ->
            io:format("~p|~p|encrypt_data|Other:~p~n", [?MODULE, ?LINE, Other]),
            {error, internal}
    end.

encrypt_data(Key, Data) ->
    io:format("~p|~p|encrypt_data|Key:~p~n", [?MODULE, ?LINE, Key]),
    case catch crypto:crypto_one_time(aes_128_ctr, Key, ?IV, Data, true) of
        {'EXIT', Exception} ->
            io:format("~p|~p|encrypt_data|Exception:~p~n", [?MODULE, ?LINE, Exception]),
            {error, internal};
        EncryptedData ->
            io:format("~p|~p|encrypt_data|EncryptedData:~p~n", [?MODULE, ?LINE, EncryptedData]),
            {ok, EncryptedData}
    end.

decrypt_data(Key, EncryptedData) ->
    io:format("~p|~p|decrypt_data|EncryptedData:~p~n", [?MODULE, ?LINE, EncryptedData]),
    case catch crypto:crypto_one_time(aes_128_ctr, Key, ?IV, EncryptedData, false) of
        {'EXIT', Exception} ->
            io:format("~p|~p|decrypt_data|Exception:~p~n", [?MODULE, ?LINE, Exception]),
            {error, internal};
        Data ->
            io:format("~p|~p|decrypt_data|Data:~p~n", [?MODULE, ?LINE, Data]),
            {ok, Data}
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

store_data(Key, DataKey, Data) ->
    io:format("store_data|Key:~p~n", [Key]),
    ok = configure_aws(erlcloud_ddb2),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    io:format("store_data|TableName:~p~n", [TableName]),
    case erlcloud_ddb2:put_item(TableName,
                                [{<<"key">>, {s, Key}},
                                 {<<"data_key">>, {s, DataKey}},
                                 {<<"data">>, {s, Data}}])
    of
        {ok, _} ->
            ok;
        Other ->
            io:format("store_data|Other:~p~n", [Other]),
            internal
    end.

retrieve_data(Key) ->
    ok = configure_aws(erlcloud_ddb2),
    io:format("~p|~p|retrieve_data|Key:~p~n", [?MODULE, ?LINE, Key]),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    io:format("~p|~p|retrieve_data|TableName:~p~n", [?MODULE, ?LINE, TableName]),
    Res = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, Key}}),
    io:format("~p|~p|retrieve_data|Res:~p~n", [?MODULE, ?LINE, Res]),
    case erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, Key}}) of
        {ok, []} ->
            {error, not_found};
        {ok, [{<<"key">>, _}, {<<"data_key">>, EncryptedDataKey}, {<<"data">>, EncryptedData}]} ->
            io:format("~p|~p|retrieve_data|EncryptedData:~p~n", [?MODULE, ?LINE, EncryptedData]),
            case decrypt_data(EncryptedDataKey) of
                {ok, DataKey} ->
                    decrypt_data(DataKey, base64:decode(EncryptedData));
                Other ->
                    io:format("~p|~p|retrieve_data|Other:~p~n", [?MODULE, ?LINE, Other]),
                    {error, internal}
            end;
        {ok, [{<<"key">>, _}, {<<"data">>, EncryptedData}, {<<"data_key">>, EncryptedDataKey}]} ->
            io:format("~p|~p|retrieve_data|EncryptedData:~p~n", [?MODULE, ?LINE, EncryptedData]),
            case decrypt_data(EncryptedDataKey) of
                {ok, DataKey} ->
                    io:format("~p|~p|retrieve_data|DataKey:~p~n", [?MODULE, ?LINE, DataKey]),
                    decrypt_data(DataKey, base64:decode(EncryptedData));
                Other ->
                    io:format("~p|~p|retrieve_data|Other:~p~n", [?MODULE, ?LINE, Other]),
                    {error, internal}
            end;
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

get_data_key() ->
    ok = miniclip_gpb_utils:configure_aws(erlcloud_kms),
    {ok, AwsKeyId} = application:get_env(miniclip_gpb, aws_kms_key_id),
    {ok, Size} = application:get_env(miniclip_gpb, aws_data_key_size),
    case erlcloud_kms:generate_data_key(AwsKeyId, [{number_of_bytes, Size}]) of
        {ok, [{<<"CiphertextBlob">>, EncryptedKey}, _, _, {<<"Plaintext">>, EncodedKey}]} ->
            io:format("~p|~p|get_data_key|EncryptedKey:~p~n", [?MODULE, ?LINE, EncryptedKey]),
            io:format("~p|~p|get_data_key|EncodedKey:~p~n", [?MODULE, ?LINE, EncodedKey]),
            {ok, {EncryptedKey, base64:decode(EncodedKey)}};
        Other ->
            io:format("~p|~p|get_data_key|Other:~p~n", [?MODULE, ?LINE, Other]),
            {error, internal}
    end.

configure_aws(Module) ->
    {ok, AccessKeyId} = application:get_env(miniclip_gpb, aws_access_key_id),
    io:format("configure_aws|AccessKeyId:~p~n", [AccessKeyId]),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    io:format("configure_aws|SecretAccessKey:~p~n", [SecretAccessKey]),
    Module:configure(AccessKeyId, SecretAccessKey),
    ok.
