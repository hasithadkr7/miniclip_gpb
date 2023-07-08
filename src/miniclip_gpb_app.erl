%%%-------------------------------------------------------------------
%% @doc miniclip_gpb public API
%% @end
%%%-------------------------------------------------------------------

-module(miniclip_gpb_app).

-behaviour(application).

-export([start/0, stop/0]).
-export([start/2, stop/1]).

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(miniclip_gpb),
    ok.

-spec stop() -> ok.
stop() ->
    application:stop(miniclip_gpb).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    ok = configure_aws(),
    miniclip_gpb_sup:start_link().

-spec stop(_State) -> ok.
stop(_State) ->
    ok.

%% internal functions

-spec configure_aws() -> ok.
configure_aws() ->
    {ok, AccessKeyID} = application:get_env(miniclip_gpb, aws_access_key_id),
    {ok, SecretAccessKey} = application:get_env(miniclip_gpb, aws_secret_access_key),
    {ok, TableName} = application:get_env(miniclip_gpb, aws_data_table_name),
    ok = erlcloud_ddb2:configure(AccessKeyID, SecretAccessKey),
    case erlcloud_ddb2:describe_table(TableName) of
        {ok, _} ->
            ok = lager:info("dynamo db table already exist~n", []),
            ok;
        {error, {<<"ResourceNotFoundException">>, _}} ->
            {ok, _} = erlcloud_ddb2:create_table(TableName, {<<"key">>, s}, <<"key">>, 1, 1)
    end,
    ok.
