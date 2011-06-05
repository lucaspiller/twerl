-module(stream_client_util_test).

-include_lib("eunit/include/eunit.hrl").

generate_auth_headers_with_custom_headers_test() ->
    Result = stream_client_util:generate_auth_headers("user", "pass", []),
    Expected = [
        {"Authorization", "Basic " ++ binary_to_list(base64:encode("user" ++ ":" ++ "pass"))}
    ],

    ?assert(Result =:= Expected).

generate_auth_headers_with_default_headers_test() ->
    Result = stream_client_util:generate_auth_headers("user", "pass"),
    Headers = stream_client_util:generate_headers(),
    Expected = [
        {"Authorization", "Basic " ++ binary_to_list(base64:encode("user" ++ ":" ++ "pass"))} | Headers
    ],

    ?assert(Result =:= Expected).

generate_headers_test() ->
    Result = stream_client_util:generate_headers(),
    Expected = [
        {"Host", "api.twitter.com"},
        {"User-Agent", "Twerl"}
    ],

    ?assert(Result =:= Expected).
