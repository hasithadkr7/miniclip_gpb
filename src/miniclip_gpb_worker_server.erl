%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2023 5:17 AM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_worker_server).

-author("hasitha").

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {worker_id, socket, key_id}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec start_link(socket:t()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Socket) ->
    WorkerId = list_to_atom(lists:concat([?MODULE, "_", rand:uniform(10000)])),
    io:format("start_link|WorkerId : ~p~n", [WorkerId]),
    gen_server:start_link({local, WorkerId}, ?MODULE, {WorkerId, Socket}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: #state{}} |
              {ok, State :: #state{}, timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init({WorkerId, Socket}) ->
    io:format("init|{WorkerId, Socket} : ~p~n", [{WorkerId, Socket}]),
    {ok, AwsKeyId} = application:get_env(miniclip_gpb, aws_kms_key_id),
    %% properly seeding the process
    %%    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed(default),
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(WorkerId, accept),
    io:format("init|accepted. ~n", []),
    {ok,
     #state{worker_id = WorkerId,
            socket = Socket,
            key_id = AwsKeyId}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(accept, #state{socket = ListenSocket, worker_id = WorkerId} = State) ->
    io:format("handle_cast|ListenSocket : ~p~n", [ListenSocket]),
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("handle_cast|AcceptSocket : ~p~n", [AcceptSocket]),
    io:format("handle_cast|accept|WorkerId : ~p~n", [WorkerId]),
    miniclip_gpb_socket_server_sup:start_socket(), % a new acceptor is born, praise the lord
    {noreply, State#state{socket = AcceptSocket}};
handle_cast({request,
             {req_envelope,
              set_request_t,
              {set_request, {data, Key, Data}},
              undefined,
              undefined,
              undefined} =
                 DecodePayload},
            #state{socket = Socket} = State) ->
    io:format("handle_cast|set_request_t|DecodePayload : ~p~n", [DecodePayload]),
    ErrorT = store_data(Key, Data),
    send_set_request_response(Socket, ErrorT),
    {noreply, State};
handle_cast({request,
             {req_envelope, get_request_t, undefined, undefined, {get_request, Key}, undefined} =
                 DecodePayload},
            #state{socket = Socket} = State) ->
    io:format("handle_cast|get_request_t|DecodePayload : ~p~n", [DecodePayload]),
    case retrieve_data(Key) of
        {ok, Data} ->
            send_get_request_response(Socket, Key, Data, ok);
        {error, ErrorT} ->
            send_get_request_response(Socket, Key, undefined, ErrorT)
    end,
    {noreply, State};
handle_cast(Request, State) ->
    io:format("handle_cast|unhandled request : ~p~n", [Request]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: #state{}}.
handle_info({tcp, _Socket, Payload}, #state{worker_id = WorkerId} = State) ->
    io:format("handle_info|tcp|Payload : ~p~n", [Payload]),
    DecodePayload = miniclip_gpb_utils:decode_request_payload(Payload),
    io:format("handle_info|DecodePayload : ~p~n", [DecodePayload]),
    io:format("handle_info|tcp|self : ~p~n", [self()]),
    gen_server:cast(WorkerId, {request, DecodePayload}),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    io:format("handle_info|tcp_closed : ~p~n", [tcp_closed]),
    {noreply, State};
handle_info({tcp_error, _Socket, _}, State) ->
    io:format("handle_info|tcp_error : ~p~n", [tcp_error]),
    {noreply, State};
handle_info(Info, State = #state{}) ->
    io:format("unexpected: ~p~n", [Info]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #state{}) ->
                   term().
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_data(Key, Data) ->
    io:format("~p|~p|store_data|Data:~p~n", [?MODULE, ?LINE, Data]),
    case miniclip_gpb_utils:encrypt_data(Data) of
        {ok, {EncryptedKey, EncryptedData}} ->
            io:format("~p|~p|store_data|EncryptedData:~p~n", [?MODULE, ?LINE, EncryptedData]),
            miniclip_gpb_utils:store_data(Key, EncryptedKey, EncryptedData);
        {error, internal} ->
            internal
    end.

retrieve_data(Key) ->
    io:format("~p|~p|retrieve_data|Key:~p~n", [?MODULE, ?LINE, Key]),
    miniclip_gpb_utils:retrieve_data(Key).

send_set_request_response(Socket, ErrorT) ->
    io:format("~p|~p|send_set_request_response|ErrorT:~p~n", [?MODULE, ?LINE, ErrorT]),
    Response = miniclip_gpb_utils:create_response({set_request, []}, ErrorT),
    miniclip_gpb_utils:send_response(Socket, Response).

send_get_request_response(Socket, Key, Data, ErrorT) ->
    io:format("~p|~p|send_get_request_response|ErrorT:~p~n", [?MODULE, ?LINE, ErrorT]),
    Response = miniclip_gpb_utils:create_response({get_request, {Key, Data}}, ErrorT),
    miniclip_gpb_utils:send_response(Socket, Response).
