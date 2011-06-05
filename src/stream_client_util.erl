-module(stream_client_util).
-export([
          generate_headers/0,
          generate_auth_headers/2,
          generate_auth_headers/3
        ]).

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
