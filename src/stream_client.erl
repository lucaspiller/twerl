-module(stream_client).
-export([
          connect/4
        ]).

-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

-spec connect(string(), list(), string(), term()) -> ok | {error, reason}.
connect(Url, Headers, PostData, Callback) ->
    case catch http:request(post, {Url, Headers, ?CONTENT_TYPE, PostData}, [], [{sync, false}, {stream, self}]) of
        {error, Reason} ->
            {error, {http_error, Reason}}
    end.
