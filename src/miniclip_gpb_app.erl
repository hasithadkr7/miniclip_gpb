%%%-------------------------------------------------------------------
%% @doc miniclip_gpb public API
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_gpb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %%    {ok, RanchPort} = application:get_env(miniclip_gpb, ranch_port),
    %%    {ok, _} =
    %%        ranch:start_listener(tcp_gpb,
    %%                             ranch_tcp,
    %%                             #{socket_opts => [{port, RanchPort}]},
    %%                             miniclip_gpb_protocol_handler,
    %%                             []),
    ok = configure_aws(),
    miniclip_gpb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

configure_aws() ->
    {ok, AccessKeyID} = application:get_env(miniclip_gpb, aws_access_key_id),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    %%    {ok, AwsKeyId} = application:get_env(miniclip_gpb, aws_kms_key_id),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    %%    ok = erlcloud_kms:configure(AccessKeyID, SecretAccessKey),
    ok = erlcloud_ddb2:configure(AccessKeyID, SecretAccessKey),
    %%    {ok, _} = erlcloud_kms:enable_key(AwsKeyId),
    case erlcloud_ddb2:describe_table(TableName) of
        {ok, _} ->
            ok;
        {error, {<<"ResourceNotFoundException">>, _}} ->
            {ok, _} = erlcloud_ddb2:create_table(TableName, {<<"key">>, s}, <<"key">>, 1, 1)
    end,
    ok.
