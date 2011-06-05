-module(stream_client).
-export([
          connect/4,
          handle_connection/2
        ]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

-spec connect(string(), list(), string(), term()) -> ok | {error, reason}.
connect(Url, Headers, PostData, Callback) ->
    case catch http:request(post, {Url, Headers, ?CONTENT_TYPE, PostData}, [], [{sync, false}, {stream, self}]) of
        {ok, RequestId} ->
            ?MODULE:handle_connection(Callback, RequestId);

        {error, Reason} ->
            {error, {http_error, Reason}}
    end.

-spec handle_connection(term(), term()) -> ok | {error, reason}.
handle_connection(_Callback, _RequestId) ->
    {error, unimplemented}.
