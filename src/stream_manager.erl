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
            NewPid = spawn_link(client_connect(State));
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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
        {client_exit, unauthorised} ->
            Pid = undefined,
            Status = {error, unauthorised};
        {client_exit, stream_end} ->
            % TODO reconnect
            Pid = undefined,
            Status = disconnected;
        {client_exit, terminate} ->
            % We closed the connection
            Pid = undefined,
            Status = disconnected;
        {client_exit, Error} ->
            % TODO maybe try reconnecting?
            Pid = undefined,
            Status = {error, Error}
    end,
    {noreply, State#state{status = Status, client_pid = Pid}}.

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
client_connect(#state{headers = Headers, params = Params, callback = StateCallback}) ->
    % Callback can be undefined, other options have defaults
    case StateCallback of
        undefined ->
            Callback = fun(_) -> ok end;
        _ ->
            Callback = StateCallback
    end,
    Url = stream_client_util:filter_url(),
    Parent = self(),
    fun() ->
        case stream_client:connect(Url, Headers, Params, Callback) of
            {error, unauthorised} ->
                % Didn't connect, unauthorised
                Parent ! {client_exit, unauthorised};
            {ok, stream_end} ->
                % Connection closed normally
                Parent ! {client_exit, stream_end};
            {ok, terminate} ->
                % Connection closed normally
                Parent ! {client_exit, terminate};
            {error, Error} ->
                % Connection closed due to error
                Parent ! {client_exit, Error}
        end
    end.

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
