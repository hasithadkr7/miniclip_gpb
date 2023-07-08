%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2023 5:16 AM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_socket_server_sup).

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
    case gen_tcp:listen(Port, [binary, {active, true}, {packet, 4}]) of
        {ok, ListenSocket} ->
            spawn_link(fun empty_listeners/0),
            {ok,
             {{simple_one_for_one, 60, 3600},
              [{socket,
                {miniclip_gpb_worker_server, start_link, [ListenSocket]}, % pass the socket!
                temporary,
                1000,
                worker,
                [miniclip_gpb_worker_server]}]}};
        Error ->
            ok = lager:error("tcp connection|Error: ~p~n", [Error]),
            ignore
    end.

-spec start_socket() -> ok.
start_socket() ->
    {ok, _} = supervisor:start_child(?MODULE, []).

-spec empty_listeners() -> ok.
empty_listeners() ->
    {ok, ListenerCount} = application:get_env(miniclip_gpb, listener_count),
    [start_socket() || _ <- lists:seq(1, ListenerCount)],
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
