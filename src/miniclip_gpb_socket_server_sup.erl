%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2023 5:16 AM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_socket_server_sup).

-author("hasitha").

-behaviour(supervisor).

%% API
-export([start_link/0, child_spec/0, start_socket/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec child_spec() -> map().
child_spec() ->
    #{id => ?MODULE,
      start => {?MODULE, start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec init(Args :: term()) ->
              {ok,
               {SupFlags ::
                    {RestartStrategy :: supervisor:strategy(),
                     MaxR :: non_neg_integer(),
                     MaxT :: non_neg_integer()},
                [ChildSpec :: supervisor:child_spec()]}} |
              ignore |
              {error, Reason :: term()}.
init([]) ->
    {ok, Port} = application:get_env(miniclip_gpb, port),
    %%    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}, {packet, line}]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn_link(fun empty_listeners/0),
    {ok,
     {{simple_one_for_one, 60, 3600},
      [{socket,
        {miniclip_gpb_worker_server, start_link, [ListenSocket]}, % pass the socket!
        temporary,
        1000,
        worker,
        [miniclip_gpb_worker_server]}]}}.

-spec start_socket() -> ok.
start_socket() ->
    io:format("start_socket : ~p~n", [?MODULE]),
    Res = supervisor:start_child(?MODULE, []),
    io:format("start_socket|Res : ~p~n", [Res]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
empty_listeners() ->
    {ok, ListenerCount} = application:get_env(miniclip_gpb, listener_count),
    [start_socket() || _ <- lists:seq(1, ListenerCount)],
    ok.
