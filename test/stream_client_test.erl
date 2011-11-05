-module(stream_client_test).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_URL, "http://localhost:4567/").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").

connect_returns_http_error_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request, fun(_, _, _, _) -> {error, something_went_wrong} end),

    Result = stream_client:connect(?TEST_URL, [], "", self()),
    Expected = {error, {http_error, something_went_wrong}},

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_post_method_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_url_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_headers_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    Headers = [a, b, c],
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_content_type_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_post_data_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    PostData = "test123",
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_other_params_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_uses_correct_client_args_test() ->
    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request,
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

    meck:unload(httpc),
    ?assert(Result =:= Expected).

connect_successful_connection_passes_to_handle_connection_test() ->
    RequestId = 1234,
    Callback = 4567,

    code:unstick_mod(httpc),
    meck:new(httpc),
    meck:expect(httpc, request, fun(_, _, _, _) -> {ok, RequestId} end),
    meck:new(stream_client, [passthrough]),
    meck:expect(stream_client, handle_connection,
        fun(PassedCallback, PassedRequestId) ->
            case {PassedCallback, PassedRequestId} of
                {Callback, RequestId} ->
                    {ok, test_ok};

                _ ->
                    {error, invalid_params}
            end
        end
    ),

    Result = stream_client:connect(?TEST_URL, [], "", Callback),
    Expected = {ok, test_ok},

    meck:unload(httpc),
    meck:unload(stream_client),

    ?assert(Result =:= Expected).

handle_connection_blocks_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    receive
        {Child, response, _Response} ->
            ?assert(unexpected_response)
    after 100 ->
        ok
    end.

handle_connection_returns_ok_and_pid_on_stream_termination_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Child ! {http, {RequestId, stream_end, []}},

    receive
        {Child, response, Response} ->
            ?assertEqual({ok, RequestId}, Response)
    after 100 ->
        ?assert(timeout)
    end.

handle_connection_passes_data_to_callback_test() ->
    Parent = self(),
    Callback = fun(CallbackData) ->
            Parent ! {self(), callback, CallbackData}
    end,
    RequestId = 1234,

    Child = spawn(fun() ->
                stream_client:handle_connection(Callback, RequestId)
        end),

    Data = <<"{\"text\":\"data1234\"}">>,
    DecodedData = [{<<"text">>, <<"data1234">>}],
    Child ! {http, {RequestId, stream, Data}},

    receive
        {CallbackPid, callback, CallbackData} ->
            ?assertNot(Child =:= CallbackPid),
            ?assertEqual(DecodedData, CallbackData)
    after 100 ->
        ?assert(timeout)
    end.

handle_connection_full_flow_test() ->
    Parent = self(),
    Callback = fun(CallbackData) ->
            Parent ! {self(), callback, CallbackData}
    end,
    RequestId = 1234,

    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Data = <<"{\"text\":\"data1234\"}">>,
    DecodedData = [{<<"text">>, <<"data1234">>}],
    Child ! {http, {RequestId, stream_start, []}},
    Child ! {http, {RequestId, stream, Data}},
    Child ! {http, {RequestId, stream_end, []}},

    receive
        {_CallbackPid, callback, CallbackData} ->
            ?assertEqual(DecodedData, CallbackData)
    after 100 ->
        ?assert(timeout_callback)
    end,

    receive
        {Child, response, Response} ->
            ?assertEqual({ok, RequestId}, Response)
    after 100 ->
        ?assert(timeout_response)
    end.

handle_connection_unauthorised_returns_error_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Child ! {http, {RequestId, {{"HTTP/1.1", 401, "Unauthorised"}, [], "Not allowed"}}},

    receive
        {Child, response, Response} ->
            ?assertEqual({error, unauthorised}, Response)
    after 100 ->
        ?assert(timeout)
    end.

handle_connection_invalid_args_returns_error_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Body = <<"Params invalid">>,
    Child ! {http, {RequestId, {{"HTTP/1.1", 406, "Not Acceptable"}, [], Body}}},

    receive
        {Child, response, Response} ->
            ?assertEqual({error, {invalid_params, Body}}, Response)
    after 100 ->
        ?assert(timeout)
    end.

handle_connection_http_error_returns_error_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Child ! {http, {RequestId, {error, {connect_failed,{ref, {error,nxdomain}}}}}},

    receive
        {Child, response, Response} ->
            ?assertEqual({error, http_error}, Response)
    after 100 ->
        ?assert(timeout)
    end.

handle_connection_terminate_returns_ok_test() ->
    Callback = fun(_Data) -> ok end,
    RequestId = 1234,

    Parent = self(),
    Child = spawn(fun() ->
                Response = stream_client:handle_connection(Callback, RequestId),
                Parent ! {self(), response, Response}
        end),

    Child ! terminate,

    receive
        {Child, response, Response} ->
            ?assertEqual({ok, RequestId}, Response)
    after 100 ->
        ?assert(timeout)
    end.
