-module(stream_client).
-export([
          connect/4,
          handle_connection/2
        ]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

-spec connect(string(), list(), string(), fun()) -> ok | {error, reason}.
connect(Url, Headers, PostData, Callback) ->
    case catch http:request(post, {Url, Headers, ?CONTENT_TYPE, PostData}, [], [{sync, false}, {stream, self}]) of
        {ok, RequestId} ->
            ?MODULE:handle_connection(Callback, RequestId);

        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

-spec handle_connection(term(), term()) -> ok | {error, reason}.
handle_connection(Callback, RequestId) ->
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            handle_connection(Callback, RequestId);

        {http, {RequestId, stream, Data}} ->
            spawn(fun() ->
                DecodedData = stream_client_util:decode(Data),
                Callback(DecodedData)
            end),
            handle_connection(Callback, RequestId);

        {http, {RequestId, stream_end, _Headers}} ->
            {ok, RequestId};

        {http, {RequestId, {{_, 401, _}, _Headers, _Body}}} ->
            {error, unauthorised};

        {http, {RequestId, {{_, 406, _}, _Headers, Body}}} ->
            {error, {invalid_params, Body}};

        {http, {RequestId, {error, _Reason}}} ->
            {error, http_error};

        terminate ->
            {ok, RequestId};

        Other ->
            error_logger:info_msg("stream_client#handle_connection received unexpected message: ~p~n", [Other]),
            handle_connection(Callback, RequestId)
    end.
