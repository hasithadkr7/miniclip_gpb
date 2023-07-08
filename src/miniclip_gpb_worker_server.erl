%%%-------------------------------------------------------------------
%%% @author hasitha
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2023 5:17 AM
%%%-------------------------------------------------------------------
-module(miniclip_gpb_worker_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {worker_id :: atom(), socket :: socket:t()}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec start_link(socket:t()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(Socket) ->
    WorkerId = list_to_atom(lists:concat([?MODULE, "_", rand:uniform(10000)])),
    gen_server:start_link({local, WorkerId}, ?MODULE, {WorkerId, Socket}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec init(Args :: term()) ->
              {ok, State :: state()} |
              {ok, State :: state(), timeout() | hibernate} |
              {stop, Reason :: term()} |
              ignore.
init({WorkerId, Socket}) ->
    ok = lager:info("socket worker ~p started ~n", [WorkerId]),
    rand:seed(default),
    gen_server:cast(WorkerId, accept),
    {ok, #state{worker_id = WorkerId, socket = Socket}}.

%% @private
%% @doc Handling call messages
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: state()) ->
                     {reply, Reply :: term(), NewState :: state()} |
                     {reply, Reply :: term(), NewState :: state(), timeout() | hibernate} |
                     {noreply, NewState :: state()} |
                     {noreply, NewState :: state(), timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
                     {stop, Reason :: term(), NewState :: state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec handle_cast(Request :: term(), State :: state()) ->
                     {noreply, NewState :: state()} |
                     {noreply, NewState :: state(), timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: state()}.
handle_cast(accept, #state{socket = ListenSocket} = State) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
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
    ok = lager:info("request received|set_request_t ~p", [DecodePayload]),
    ErrorT = store_data(Key, Data),
    send_set_request_response(Socket, ErrorT),
    {noreply, State};
handle_cast({request,
             {req_envelope, get_request_t, undefined, undefined, {get_request, Key}, undefined} =
                 DecodePayload},
            #state{socket = Socket} = State) ->
    ok = lager:info("request received|get_request_t ~p", [DecodePayload]),
    case retrieve_data(Key) of
        {ok, Data} ->
            send_get_request_response(Socket, Key, Data, ok);
        {error, ErrorT} ->
            send_get_request_response(Socket, Key, undefined, ErrorT)
    end,
    {noreply, State};
handle_cast(Request, State) ->
    ok = lager:warning("unhandled request received|Request ~p", [Request]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: timeout() | term(), State :: state()) ->
                     {noreply, NewState :: state()} |
                     {noreply, NewState :: state(), timeout() | hibernate} |
                     {stop, Reason :: term(), NewState :: state()}.
handle_info({tcp, _Socket, Payload}, #state{worker_id = WorkerId} = State) ->
    ok = lager:info("request received|Payload ~p", [Payload]),
    DecodePayload = miniclip_gpb_utils:decode_request_payload(Payload),
    gen_server:cast(WorkerId, {request, DecodePayload}),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    ok = lager:info("received|tcp_closed", []),
    {noreply, State};
handle_info({tcp_error, _Socket, _}, State) ->
    ok = lager:info("received|tcp_error", []),
    {noreply, State};
handle_info(Info, State) ->
    ok = lager:info("unexpected info received|Info ~p", [Info]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: state()) ->
                   term().
terminate(Reason, _State) ->
    ok = lager:warning("worker terminated|Reason ~p", [Reason]),
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: term() | {down, term()}, State :: state(), Extra :: term()) ->
                     {ok, NewState :: state()} | {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

store_data(Key, Data) ->
    case miniclip_gpb_utils:encrypt_data(Data) of
        {ok, {EncryptedKey, EncryptedData}} ->
            miniclip_gpb_utils:store_data(Key, EncryptedKey, EncryptedData);
        {error, internal} ->
            internal
    end.

retrieve_data(Key) ->
    miniclip_gpb_utils:retrieve_data(Key).

send_set_request_response(Socket, ErrorT) ->
    ok = lager:debug("send_set_request_response|ErrorT ~p", [ErrorT]),
    Response = miniclip_gpb_utils:create_response({set_request, []}, ErrorT),
    ok = lager:debug("send_set_request_response|Response ~p", [Response]),
    miniclip_gpb_utils:send_response(Socket, Response).

send_get_request_response(Socket, Key, Data, ErrorT) ->
    ok = lager:debug("send_get_request_response|ErrorT ~p", [ErrorT]),
    Response = miniclip_gpb_utils:create_response({get_request, {Key, Data}}, ErrorT),
    ok = lager:debug("send_get_request_response|Response ~p", [Response]),
    miniclip_gpb_utils:send_response(Socket, Response).
