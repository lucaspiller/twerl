-module(stream_manager).
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
        handle_call/3, handle_cast/2,
        handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([
          start_link/0,
          start_link/1,
          stop/1,
          start_stream/1,
          stop_stream/1,
          set_params/2,
          set_callback/2,
          set_headers/2,
          status/1
        ]).

-record(state, {
        status = disconnected :: atom(),
        headers = [] :: list(),
        params = "" :: string(),
        callback :: term(),
        client_pid :: pid()
    }).

%%====================================================================
%% api callbacks
%%====================================================================
start_link() ->
    start_link(?MODULE).

start_link(GenServerName) ->
    Args = [],
    gen_server:start_link({local, GenServerName}, ?MODULE, Args, []).

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

start_stream(ServerRef) ->
    gen_server:call(ServerRef, start_stream).

stop_stream(ServerRef) ->
    gen_server:call(ServerRef, stop_stream).

set_params(ServerRef, Params) ->
    gen_server:call(ServerRef, {set_params, Params}).

set_callback(ServerRef, Callback) ->
    gen_server:call(ServerRef, {set_callback, Callback}).

set_headers(ServerRef, Headers) ->
    gen_server:call(ServerRef, {set_headers, Headers}).

status(ServerRef) ->
    gen_server:call(ServerRef, status).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    ok = client_shutdown(State),
    {stop, normal, stopped, State};

handle_call(start_stream, _From, State = #state{client_pid = Pid}) ->
    case Pid of
        undefined ->
            % not started, start client
            NewPid = client_connect(State);
        _ ->
            % alrady started, ignore
            NewPid = Pid
    end,
    {reply, ok, State#state{ client_pid = NewPid, status = connected }};

handle_call(stop_stream, _From, State) ->
    ok = client_shutdown(State),
    % we leave the state as is, we will get a message when the client ends
    % and set the pid / status there
    {reply, ok, State};

handle_call({set_params, Params}, _From, State = #state{client_pid = Pid, params = OldParams}) ->
    case Params of
        OldParams ->
            % same, don't do anything
            {reply, ok, State};
        _ ->
            % different, change and see if we need to restart the client
            case Pid of
                undefined ->
                    % not started, nothing to do
                    NewPid = Pid;
                _ ->
                    % already started, restart
                    ok = client_shutdown(State),
                    NewPid = client_connect(State#state{ params = Params })
            end,
            {reply, ok, State#state{ params = Params, client_pid = NewPid }}
    end;

handle_call({set_headers, Headers}, _From, State = #state{client_pid = Pid, headers = OldHeaders}) ->
    case Headers of
        OldHeaders ->
            % same, don't do anything
            {reply, ok, State};
        _ ->
            % different, change and see if we need to restart the client
            case Pid of
                undefined ->
                    % not started, nothing to do
                    NewPid = Pid;
                _ ->
                    % already started, restart
                    ok = client_shutdown(State),
                    NewPid = client_connect(State#state{ headers = Headers })
            end,
            {reply, ok, State#state{ headers = Headers, client_pid = NewPid }}
    end;

handle_call({set_callback, Callback}, _From, State) ->
    {reply, ok, State#state{ callback = Callback }};

handle_call(status, _From, State = #state{status = Status}) ->
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({client_data, Data}, State = #state{ callback = Callback }) ->
    case Callback of
        undefined ->
            % no callback set, ignore data
            ok;
        _ ->
            % callback set, call with data
            spawn(fun() -> Callback(Data) end)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

% we only care about messages from the current client, not old shutdown message
handle_info({Pid, client_exit, Message}, State) when Pid == State#state.client_pid ->
    case Message of
        % Handle messages from client process terminating
        unauthorised ->
            NewPid = undefined,
            Status = {error, unauthorised};
        stream_end ->
            % TODO reconnect
            NewPid = undefined,
            Status = disconnected;
        terminate ->
            % We closed the connection
            NewPid = undefined,
            Status = disconnected;
        Error ->
            % TODO maybe try reconnecting?
            NewPid = undefined,
            Status = {error, Error}
    end,
    {noreply, State#state{status = Status, client_pid = NewPid}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec client_connect(record()) -> pid().
client_connect(#state{headers = Headers, params = Params}) ->
    Parent = self(),

    % We don't use the callback from the state, as we want to be able to change
    % it without restarting the client. As such we call back into the manager
    % which deals with the data as it sees fit
    Callback = fun(Data) ->
        gen_server:cast(Parent, {client_data, Data})
    end,

    Endpoint = {post, stream_client_util:filter_url()},
    spawn_link(fun() ->
        case stream_client:connect(Endpoint, Headers, Params, Callback) of
            {error, unauthorised} ->
                % Didn't connect, unauthorised
                Parent ! {self(), client_exit, unauthorised};
            {ok, stream_end} ->
                % Connection closed normally
                Parent ! {self(), client_exit, stream_end};
            {ok, terminate} ->
                % Connection closed normally
                Parent ! {self(), client_exit, terminate};
            {error, Error} ->
                % Connection closed due to error
                Parent ! {self(), client_exit, Error}
        end
    end).

-spec client_shutdown(record()) -> ok.
client_shutdown(#state{client_pid = Pid}) ->
    case Pid of
        undefined ->
            % not started, nothing to do
            ok;
        _ ->
            % terminate the client
            Pid ! terminate,
            ok
    end.
