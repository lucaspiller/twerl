-module(stream_client).
-export([
          connect/4,
          handle_connection/2
        ]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

-spec connect(string(), list(), string(), fun()) -> ok | {error, reason}.
connect(Url, Headers, PostData, Callback) ->
    case catch httpc:request(post, {Url, Headers, ?CONTENT_TYPE, PostData}, [], [{sync, false}, {stream, self}]) of
        {ok, RequestId} ->
            ?MODULE:handle_connection(Callback, RequestId);

        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

-spec handle_connection(term(), term()) -> ok | {error, term()}.
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
            {ok, RequestId};

        % connected but received error cod
        % 401 unauthorised - authentication credentials rejected
        {http, {RequestId, {{_, 401, _}, _Headers, _Body}}} ->
            {error, unauthorised};

        % 406 not acceptable - invalid request to the search api
        {http, {RequestId, {{_, 406, _}, _Headers, Body}}} ->
            {error, {invalid_params, Body}};

        % connection error
        % may happen while connecting or after connected
        {http, {RequestId, {error, _Reason}}} ->
            {error, http_error};

        % received terminate message
        terminate ->
            {ok, RequestId}
    end.
