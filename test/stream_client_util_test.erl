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

userids_to_follow_one_userid_test() ->
    Result = stream_client_util:userids_to_follow(["1"]),
    Expected = {ok, "follow=1"},

    ?assert(Result =:= Expected).

userids_to_follow_two_userids_test() ->
    Result = stream_client_util:userids_to_follow(["1", "2"]),
    Expected = {ok, "follow=1,2"},

    ?assert(Result =:= Expected).

userids_to_follow_no_userids_test() ->
    Result = stream_client_util:userids_to_follow([]),
    Expected = {error, no_args_passed},

    ?assert(Result =:= Expected).

keywords_to_track_one_userid_test() ->
    Result = stream_client_util:keywords_to_track(["1"]),
    Expected = {ok, "track=1"},

    ?assert(Result =:= Expected).

keywords_to_track_two_userids_test() ->
    Result = stream_client_util:keywords_to_track(["1", "2"]),
    Expected = {ok, "track=1,2"},

    ?assert(Result =:= Expected).

keywords_to_track_no_userids_test() ->
    Result = stream_client_util:keywords_to_track([]),
    Expected = {error, no_args_passed},

    ?assert(Result =:= Expected).

decode_test() ->
    Data = <<"{\"name\":\"Bob\"}">>,
    Expected = [{<<"name">>, <<"Bob">>}],
    Result = stream_client_util:decode(Data),

    ?assertEqual(Expected, Result).
