-module(stream_client).
-export([
          connect/4,
          handle_connection/2
        ]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

-spec connect(string(), list(), string(), fun()) -> ok | {error, reason}.
connect({post, Url}, Headers, Params, Callback) ->
    case catch httpc:request(post, {Url, Headers, ?CONTENT_TYPE, Params}, [], [{sync, false}, {stream, self}]) of
        {ok, RequestId} ->
            ?MODULE:handle_connection(Callback, RequestId);

        {error, Reason} ->
            {error, {http_error, Reason}}
    end;

connect({get, BaseUrl}, Headers, Params, Callback) ->
    Url = case Params of
              "" ->
                  BaseUrl;
              _ ->
                  BaseUrl ++ "?" ++ Params
          end,
    case catch httpc:request(get, {Url, Headers}, [], [{sync, false}, {stream, self}]) of
        {ok, RequestId} ->
            ?MODULE:handle_connection(Callback, RequestId);

        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

% TODO maybe change {ok, stream_closed} to an error?
-spec handle_connection(term(), term()) -> {ok, terminate} | {ok, stream_closed} | {error, term()}.
handle_connection(Callback, RequestId) ->
    receive
        % stream opened
        {http, {RequestId, stream_start, _Headers}} ->
            handle_connection(Callback, RequestId);

        % stream received data
        {http, {RequestId, stream, Data}} ->
            spawn(fun() ->
                DecodedData = stream_client_util:decode(Data),
                Callback(DecodedData)
            end),
            handle_connection(Callback, RequestId);

        % stream closed
        {http, {RequestId, stream_end, _Headers}} ->
            {ok, stream_end};

        % connected but received error cod
        % 401 unauthorised - authentication credentials rejected
        {http, {RequestId, {{_, 401, _}, _Headers, _Body}}} ->
            {error, unauthorised};

        % 406 not acceptable - invalid request to the search api
        {http, {RequestId, {{_, 406, _}, _Headers, Body}}} ->
            {error, {invalid_params, Body}};

        % connection error
        % may happen while connecting or after connected
        {http, {RequestId, {error, Reason}}} ->
            {error, {http_error, Reason}};

        % message send by us to close the connection
        terminate ->
            {ok, terminate}
    end.
