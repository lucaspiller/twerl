-module(stream_client_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_URL, "http://localhost:4567/").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

connect_returns_http_error_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request, fun(_, _, _, _) -> {error, something_went_wrong} end),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, something_went_wrong}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_post_method_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request,
        fun(Method, _, _, _) ->
            case Method of
                post ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_url_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request,
        fun(_, Args, _, _) ->
            case Args of
                {?TEST_URL, _, _, _} ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_headers_test() ->
    code:unstick_mod(http),
    meck:new(http),
    Headers = [a, b, c],
    meck:expect(http, request,
        fun(_, Args, _, _) ->
            case Args of
                {_, Headers, _, _} ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, Headers, "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_content_type_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request,
        fun(_, Args, _, _) ->
            case Args of
                {_, _, ?CONTENT_TYPE, _} ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_post_data_test() ->
    code:unstick_mod(http),
    meck:new(http),
    PostData = "test123",
    meck:expect(http, request,
        fun(_, Args, _, _) ->
            case Args of
                {_, _, _, PostData} ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, "", PostData, self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_other_params_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request,
        fun(_, _, Args, _) ->
            case Args of
                [] ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).

connect_uses_correct_client_args_test() ->
    code:unstick_mod(http),
    meck:new(http),
    meck:expect(http, request,
        fun(_, _, _, Args) ->
            case Args of
                [{sync, false}, {stream, self}] ->
                    {error, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, test_ok}},

    meck:unload(http),
    ?assert(Result =:= Expected).
