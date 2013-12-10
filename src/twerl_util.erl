-module(twerl_util).
-export([
          headers_for_auth/3,
          generate_headers/0,
          generate_auth_headers/2,
          generate_auth_headers/3,
          userids_to_follow/1,
          keywords_to_track/1,
          filter_url/0,
          decode/1
        ]).

-spec headers_for_auth(term(), term(), list()) -> list() | {list(), string()}.
headers_for_auth({basic, [User, Pass]}, _Endpoint, _Params) ->
    generate_auth_headers(User, Pass);

headers_for_auth({oauth, [ConsumerKey, ConsumerSecret, TokenKey, TokenSecret]}, _Endpoint, _Params) ->
    {[], oauth_params(ConsumerKey, ConsumerSecret, TokenKey, TokenSecret, _Endpoint, _Params)}.

-spec generate_headers() -> list().
generate_headers() ->
    [
        {"Host", "api.twitter.com"},
        {"User-Agent", "Twerl"}
    ].

-spec generate_auth_headers(string(), string()) -> list().
generate_auth_headers(User, Pass) ->
    generate_auth_headers(User, Pass, generate_headers()).

-spec generate_auth_headers(string(), string(), list()) -> list().
generate_auth_headers(User, Pass, Headers) ->
    Basic = "Basic " ++ binary_to_list(base64:encode(User ++ ":" ++ Pass)),
    [
        {"Authorization", Basic} | Headers
    ].

-spec userids_to_follow(list()) -> {ok, string()} | {error, reason}.
userids_to_follow(UserIds) ->
    args_to_params("follow", UserIds).

-spec keywords_to_track(list()) -> {ok, string()} | {error, reason}.
keywords_to_track(Keywords) ->
    args_to_params("track", Keywords).

-spec args_to_params(string(), list()) -> {ok, string()} | {error, reason}.
args_to_params(_Method, []) ->
    {error, no_args_passed};

args_to_params(Method, [Current | Remaining]) ->
    Acc = Method ++ "=" ++ Current,
    args_to_params(Method, Remaining, Acc).

args_to_params(_Method, [], Acc) ->
    {ok, Acc};

args_to_params(Method, [Current | Remaining], Acc) ->
    NewAcc = Acc ++ "," ++ Current,
    args_to_params(Method, Remaining, NewAcc).

-spec filter_url() -> string().
filter_url() ->
    "https://stream.twitter.com/1.1/statuses/filter.json".

-spec decode(binary()) -> list().
decode(Data) ->
    case Data of
        <<"\r\n">> ->
            [];
        _ ->
            {Decoded} = jiffy:decode(Data),
            Decoded
    end.

oauth_params(ConsumerKey, ConsumerSecret, TokenKey, TokenSecret, {Method, URL}, Params) ->
    MethodStr = case Method of
                    get -> "GET";
                    post -> "POST" end,
    Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
    SignedParams = oauth:sign(MethodStr, URL, Params, Consumer, TokenKey, TokenSecret),
    oauth:uri_params_encode(SignedParams).
