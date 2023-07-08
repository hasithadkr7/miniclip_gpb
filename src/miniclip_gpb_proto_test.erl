%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jun 2023 4:18 PM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_proto_test).

%% API
-export([encode_set_request_envelope/2, encode_get_request_envelope/1, decode_envelope/1,
         aws_kms_test/0, aws_kms_key_test/0, aws_kms_encrypt_decrypt_test/0, aws_dynamo_test/0,
         aws_dynamo_db_insert_get_test/0, aws_dynamo_db_get_test/1]).

% This imports the record for you to use from the generated file
-include("kv_pb.hrl").

encode_set_request_envelope(Key, Value) ->
    ok = lager:debug("encode_set_request_envelope|[Key, Value]:~p~n", [[Key, Value]]),
    %%  Req = #req_envelope{type = "set_request", set_req = #set_request{req = #data{key = "name", value = "hasitha dhananjaya adikari"}}},
    Req = #req_envelope{type = set_request_t,
                        set_req = #set_request{req = #data{key = Key, value = Value}}},
    kv_pb:encode_msg(Req, req_envelope).

encode_get_request_envelope(Key) ->
    ok = lager:debug("encode_get_request_envelope|Key:~p~n", [Key]),
    %%  Req = #req_envelope{type = "set_request", set_req = #set_request{req = #data{key = "name", value = "hasitha dhananjaya adikari"}}},
    Req = #req_envelope{type = get_request_t, get_req = #get_request{key = Key}},
    kv_pb:encode_msg(Req, req_envelope).

decode_envelope(EncodedData) ->
    Message = kv_pb:decode_msg(EncodedData, req_envelope),
    ok = lager:debug("decode_envelope|Message:~p~n", [Message]).

aws_kms_test() ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_kms:configure(AccessKeyID, SecretAccessKey),
    Key = erlcloud_kms:create_key(),
    ok = lager:debug("aws_kms_test|Key:~p~n", [Key]),
    %%  Encrypted = erlcloud_kms:encrypt(AccessKeyID, "my data secret"),
    %%  ok = lager:debug("aws_kms_test|Encrypted:~p~n", [Encrypted]),
    %%  Decrypted = erlcloud_kms:decrypt(Encrypted),
    %%  ok = lager:debug("aws_kms_test|Decrypted:~p~n", [Decrypted]),
    ok.

%%aws_kms_test|Key:{ok,[{<<"KeyMetadata">>,
%%                       [{<<"AWSAccountId">>,<<"846553054321">>},
%%                        {<<"Arn">>,
%%                         <<"arn:aws:kms:us-east-1:846553054321:key/b29d8878-3eb1-416d-8fdf-5263cb62681d">>},
%%                        {<<"CreationDate">>,1688155415.663},
%%                        {<<"CustomerMasterKeySpec">>,<<"SYMMETRIC_DEFAULT">>},
%%                        {<<"Description">>,<<>>},
%%                        {<<"Enabled">>,true},
%%                        {<<"EncryptionAlgorithms">>,[<<"SYMMETRIC_DEFAULT">>]},
%%                        {<<"KeyId">>,
%%                         <<"b29d8878-3eb1-416d-8fdf-5263cb62681d">>},
%%                        {<<"KeyManager">>,<<"CUSTOMER">>},
%%                        {<<"KeySpec">>,<<"SYMMETRIC_DEFAULT">>},
%%                        {<<"KeyState">>,<<"Enabled">>},
%%                        {<<"KeyUsage">>,<<"ENCRYPT_DECRYPT">>},
%%                        {<<"MultiRegion">>,false},
%%                        {<<"Origin">>,<<"AWS_KMS">>}]}]}
aws_kms_key_test() ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_kms:configure(AccessKeyID, SecretAccessKey),
    Result = erlcloud_kms:enable_key(<<"b29d8878-3eb1-416d-8fdf-5263cb62681d">>),
    ok = lager:debug("aws_kms_test|Result:~p~n", [Result]).

aws_kms_encrypt_decrypt_test() ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_kms:configure(AccessKeyID, SecretAccessKey),
    KeyId = <<"b29d8878-3eb1-416d-8fdf-5263cb62681d">>,
    {ok, [{<<"CiphertextBlob">>, CiphertextBlob}, _, _]} =
        erlcloud_kms:encrypt(KeyId, base64:encode("Shalika nadeeshani ekanayake")),
    ok = lager:debug("aws_kms_test|CiphertextBlob:~p~n", [CiphertextBlob]),
    {ok, [_, _, _, {<<"Plaintext">>, Decrypted}]} = erlcloud_kms:decrypt(CiphertextBlob),
    ok = lager:debug("aws_kms_test|Decrypted:~p~n", [base64:decode(Decrypted)]),
    ok.

aws_dynamo_test() ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_ddb2:configure(AccessKeyID, SecretAccessKey),
    ok = lager:debug("aws_dynamo_test|tables : ~p~n", [erlcloud_ddb2:list_tables()]),
    Result = erlcloud_ddb2:create_table(<<"test_table">>, {<<"key">>, s}, <<"key">>, 1, 1),
    ok = lager:debug("aws_dynamo_test|Result : ~p~n", [Result]),
    ok = lager:debug("aws_dynamo_test|tables : ~p~n", [erlcloud_ddb2:list_tables()]),
    ok.

aws_dynamo_db_insert_get_test() ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_ddb2:configure(AccessKeyID, SecretAccessKey),
    {ok, [TableName]} = erlcloud_ddb2:list_tables(),
    ok = lager:debug("aws_dynamo_test|tables : ~p~n", [erlcloud_ddb2:list_tables()]),
    Put = erlcloud_ddb2:put_item(TableName,
                                 [{<<"key">>, {s, "test_key1"}},
                                  {<<"data">>, {s, "my test data for the xxxxxxxxxxxxxxx"}}]),
    ok = lager:debug("aws_dynamo_test|Put : ~p~n", [Put]),
    Put2 =
        erlcloud_ddb2:put_item(TableName,
                               [{<<"key">>, {s, "test_key2"}},
                                {<<"data">>, {s, "my test data for the yyyyyyyyyyyyyyyyyyy"}}]),
    ok = lager:debug("aws_dynamo_test|Put2 : ~p~n", [Put2]),
    Put3 =
        erlcloud_ddb2:put_item(TableName,
                               [{<<"key">>, {s, "test_key3"}},
                                {<<"data">>,
                                 {s, "my test data for the xxxxxxxxxxxxxrrrrrrrrrrrrr"}}]),
    ok = lager:debug("aws_dynamo_test|Put3 : ~p~n", [Put3]),
    Put4 =
        erlcloud_ddb2:put_item(TableName,
                               [{<<"key">>, {s, "test_key4"}},
                                {<<"data">>, {s, "my test data for the zzzzzzzzzzzzzzzzzzzz"}}]),
    ok = lager:debug("aws_dynamo_test|Put4 : ~p~n", [Put4]),

    Get = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key"}}),
    ok = lager:debug("aws_dynamo_test|Get : ~p~n", [Get]),
    Get1 = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key1"}}),
    ok = lager:debug("aws_dynamo_test|Get1 : ~p~n", [Get1]),
    Get2 = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key2"}}),
    ok = lager:debug("aws_dynamo_test|Get2 : ~p~n", [Get2]),
    Get3 = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key3"}}),
    ok = lager:debug("aws_dynamo_test|Get3 : ~p~n", [Get3]),
    Get4 = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key4"}}),
    ok = lager:debug("aws_dynamo_test|Get4 : ~p~n", [Get4]),
    Get5 = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, "test_key5"}}),
    ok = lager:debug("aws_dynamo_test|Get5 : ~p~n", [Get5]),
    ok.

aws_dynamo_db_get_test(Key) ->
    AccessKeyID = "AKIA4KGT6PRYTVVWRCPF",
    SecretAccessKey = "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",
    erlcloud_ddb2:configure(AccessKeyID, SecretAccessKey),
    {ok, [TableName]} = erlcloud_ddb2:list_tables(),
    ok = lager:debug("aws_dynamo_test|tables : ~p~n", [erlcloud_ddb2:list_tables()]),
    Get = erlcloud_ddb2:get_item(TableName, {<<"key">>, {s, Key}}),
    ok = lager:debug("aws_dynamo_test|Get : ~p~n", [Get]).
