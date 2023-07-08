%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2023 2:42 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_utils).

-include("kv_pb.hrl").

-define(IV, <<0:128>>).

%% API
-export([decode_request_payload/1, encrypt_data/2, decrypt_data/1, store_data/3,
         retrieve_data/1, create_response/2, send_response/2, configure_aws/1, get_data_key/0,
         encrypt_data/1]).

-spec decode_request_payload(binary()) -> ok | {error, pw_error:error_opts()}.
decode_request_payload(EncodedData) ->
    kv_pb:decode_msg(EncodedData, req_envelope).

encrypt_data(Data) ->
    case get_data_key() of
        {ok, {EncryptedKey, Key}} ->
            case encrypt_data(Key, Data) of
                {ok, EncryptedData} ->
                    {ok, {EncryptedKey, base64:encode(EncryptedData)}};
                Error ->
                    ok = lager:error("data encryption error|Error: ~p~n", [Error]),
                    {error, internal}
            end;
        Error ->
            ok = lager:error("aws data ken gen error|Error: ~p~n", [Error]),
            {error, internal}
    end.

encrypt_data(Key, Data) ->
    try
        EncryptedData = crypto:crypto_one_time(aes_128_ctr, Key, ?IV, Data, true),
        {ok, EncryptedData}
    catch
        _:Exception ->
            ok = lager:error("crypto data encrypt error|Exception: ~p~n", [Exception]),
            {error, internal}
    end.

decrypt_data(Key, EncryptedData) ->
    try
        Data = crypto:crypto_one_time(aes_128_ctr, Key, ?IV, EncryptedData, false),
        {ok, Data}
    catch
        _:Exception ->
            ok = lager:error("crypto data decrypt error|Exception: ~p~n", [Exception]),
            {error, internal}
    end.

decrypt_data(EncryptedData) ->
    ok = configure_aws(erlcloud_kms),
    case erlcloud_kms:decrypt(EncryptedData) of
        {ok, [_, _, _, {<<"Plaintext">>, Data}]} ->
            {ok, base64:decode(Data)};
        Error ->
            ok = lager:error("aws kms decrypt data|Error: ~p~n", [Error]),
            {error, internal}
    end.

store_data(Key, DataKey, Data) ->
    ok = configure_aws(erlcloud_ddb2),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    case erlcloud_ddb2:put_item(TableName,
                                [{<<"key">>, {s, Key}},
                                 {<<"data_key">>, {s, DataKey}},
                                 {<<"data">>, {s, Data}}])
    of
        {ok, _} ->
            ok;
        Error ->
            ok = lager:error("aws store data|Error: ~p~n", [Error]),
            internal
    end.

retrieve_data(Key) ->
    ok = configure_aws(erlcloud_ddb2),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    case erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, Key}}) of
        {ok, []} ->
            ok = lager:info("data for key ~p not found ~n", [Key]),
            {error, not_found};
        {ok, [{<<"key">>, _}, {<<"data">>, EncryptedData}, {<<"data_key">>, EncryptedDataKey}]} ->
            case decrypt_data(EncryptedDataKey) of
                {ok, DataKey} ->
                    decrypt_data(DataKey, base64:decode(EncryptedData));
                Error ->
                    ok = lager:error("data decryption|Error: ~p~n", [Error]),
                    {error, internal}
            end;
        Error ->
            ok = lager:error("data retrieval|Error: ~p~n", [Error]),
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
    EncodedResponse = kv_pb:encode_msg(Response, req_envelope),
    case gen_tcp:send(Socket, EncodedResponse) of
        ok ->
            ok;
        {error, Reason} ->
            ok = lager:error("sending response error|Reason: ~p~n", [Reason])
    end.

get_data_key() ->
    ok = miniclip_gpb_utils:configure_aws(erlcloud_kms),
    {ok, AwsKeyId} = application:get_env(miniclip_gpb, aws_kms_key_id),
    {ok, Size} = application:get_env(miniclip_gpb, aws_data_key_size),
    case erlcloud_kms:generate_data_key(AwsKeyId, [{number_of_bytes, Size}]) of
        {ok, [{<<"CiphertextBlob">>, EncryptedKey}, _, _, {<<"Plaintext">>, EncodedKey}]} ->
            {ok, {EncryptedKey, base64:decode(EncodedKey)}};
        Error ->
            ok = lager:error("aws data key generation|Error: ~p~n", [Error]),
            {error, internal}
    end.

configure_aws(erlcloud_kms) ->
    {ok, AccessKeyId} = application:get_env(miniclip_gpb, aws_access_key_id),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    erlcloud_kms:configure(AccessKeyId, SecretAccessKey),
    ok;
configure_aws(erlcloud_ddb2) ->
    {ok, AccessKeyId} = application:get_env(miniclip_gpb, aws_access_key_id),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    erlcloud_ddb2:configure(AccessKeyId, SecretAccessKey),
    ok.
