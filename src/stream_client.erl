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
        {http,{RequestId, stream_start, _Headers}} ->
            handle_connection(Callback, RequestId);

        {http,{RequestId, stream, Data}} ->
            _CallbackPid = spawn(fun() -> Callback(Data) end),
            handle_connection(Callback, RequestId);

        {http,{RequestId, stream_end, _Headers}} ->
            {ok, RequestId};

        Other ->
            error_logger:info_msg("stream_client#handle_connection received unexpected message: ~p~n", [Other]),
            handle_connection(Callback, RequestId)
    end.
