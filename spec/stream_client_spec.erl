-module(stream_client_spec).
-include("espec.hrl").

-define(POST_ENDPOINT, {post, "http://localhost:4567/"}).
-define(GET_ENDPOINT, {get, "http://localhost:4567/"}).
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(TEST_AUTH, {basic, ["", ""]}).

spec() ->
   describe("stream client", fun() ->
       describe("#connect", fun() ->
               describe("http client setup", fun() ->
                       before_all(fun() ->
                                   ok = meck:new(httpc, [passthrough])
                           end),

                       after_each(fun() ->
                                   ?assertEqual(true, meck:validate(httpc))
                           end),

                       after_all(fun() ->
                                   ok = meck:unload(httpc)
                           end),

                       describe("POST endpoint", fun() ->
                           it("should return http errors", fun() ->
                                       meck:expect(httpc, request, fun(_, _, _, _) -> {error, something_went_wrong} end),

                                       Result = stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self()),
                                       Expected = {error, {http_error, something_went_wrong}},

                                       ?assertEqual(Expected, Result)
                               end),

                           it("should use http post method", fun() ->
                                       meck:expect(httpc, request,
                                           fun(Method, _, _, _) ->
                                                   ?assertEqual(post, Method),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct url", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, Args, _, _) ->
                                                   {Url, _, _, _} = Args,
                                                   {post, ExpectedUrl} = ?POST_ENDPOINT,
                                                   ?assertEqual(ExpectedUrl, Url),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct content type", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, Args, _, _) ->
                                                   {_, _, ContentType, _} = Args,
                                                   ?assertEqual(?CONTENT_TYPE, ContentType),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct params", fun() ->
                                       PostData = "test123",

                                       meck:expect(httpc, request,
                                           fun(_, Args, _, _) ->
                                                   {_, _, _, PassedPostData} = Args,
                                                   ?assertEqual(PostData, PassedPostData),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, PostData, self())

                               end),

                           % TODO check what this argument is actually for
                           it("should use the correct other params", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, _, OtherParams, _) ->
                                                   ?assertEqual([], OtherParams),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct http client arguments for streaming", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, _, _, ClientArgs) ->
                                                   ?assertEqual([{sync, false}, {stream, self}], ClientArgs),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", self())
                               end)
                       end),

                       describe("GET endpoint", fun() ->
                           it("should return http errors", fun() ->
                                       meck:expect(httpc, request, fun(_, _, _, _) -> {error, something_went_wrong} end),

                                       Result = stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, "", self()),
                                       Expected = {error, {http_error, something_went_wrong}},

                                       ?assertEqual(Expected, Result)
                               end),

                           it("should use http get method", fun() ->
                                       meck:expect(httpc, request,
                                           fun(Method, _, _, _) ->
                                                   ?assertEqual(get, Method),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct url", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, Args, _, _) ->
                                                   {Url, _} = Args,
                                                   {get, ExpectedUrl} = ?GET_ENDPOINT,
                                                   ?assertEqual(ExpectedUrl, Url),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct params", fun() ->
                                       PostData = "test123",

                                       meck:expect(httpc, request,
                                           fun(_, Args, _, _) ->
                                                   {Url, _} = Args,
                                                   {get, ExpectedUrl} = ?GET_ENDPOINT,
                                                   ?assertEqual(ExpectedUrl ++ "?" ++ PostData, Url),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, PostData, self())

                               end),

                           % TODO check what this argument is actually for
                           it("should use the correct other params", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, _, OtherParams, _) ->
                                                   ?assertEqual([], OtherParams),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, "", self())
                               end),

                           it("should use the correct http client arguments for streaming", fun() ->
                                       meck:expect(httpc, request,
                                           fun(_, _, _, ClientArgs) ->
                                                   ?assertEqual([{sync, false}, {stream, self}], ClientArgs),
                                                   {error, no_continue} % what the client expects
                                           end
                                       ),

                                       stream_client:connect(?GET_ENDPOINT, ?TEST_AUTH, "", self())
                               end)
                        end)
                   end),

               describe("connection handler", fun() ->
                       before_each(fun() ->
                                   catch meck:new(httpc, [unstick]),
                                   catch meck:new(stream_client, [passthrough])
                           end),

                       after_each(fun() ->
                                   ?assertEqual(true, meck:validate(httpc)),
                                   ?assertEqual(true, meck:validate(stream_client)),
                                   meck:unload(stream_client),
                                   meck:unload(httpc)
                           end),

                       it("should have the client passed when connected", fun() ->
                                   RequestId = 1234,
                                   Callback = 4567,

                                   meck:expect(httpc, request, fun(_, _, _, _) -> {ok, RequestId} end),
                                   meck:expect(stream_client, handle_connection,
                                       fun(PassedCallback, PassedRequestId) ->
                                               ?assertEqual(Callback, PassedCallback),
                                               ?assertEqual(RequestId, PassedRequestId),
                                               {ok, test}
                                       end),

                                   stream_client:connect(?POST_ENDPOINT, ?TEST_AUTH, "", Callback)
                           end)
                   end)
           end),

       describe("#handle_connection", fun() ->
               it("should block when connected", fun() ->
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
                           end
                   end),

               it("should return ok and the pid when the stream ends", fun() ->
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
                                   ?assertEqual({ok, stream_end}, Response)
                           after 100 ->
                                   ?assert(timeout)
                           end

                   end),

               it("should pass data to the callback", fun() ->
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
                           end
                   end),

               it("should handle the full flow correctly", fun() ->
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
                                   ?assertEqual({ok, stream_end}, Response)
                           after 100 ->
                                   ?assert(timeout_response)
                           end
                   end),

               it("should return an error when the connection is unauthorised", fun() ->
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
                           end
                   end),

               it("should return an error when stream invalid arguments are passed", fun() ->
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
                           end
                   end),

               it("should return an error when a http error occurs", fun() ->
                           Callback = fun(_Data) -> ok end,
                           RequestId = 1234,

                           Parent = self(),
                           Child = spawn(fun() ->
                                       Response = stream_client:handle_connection(Callback, RequestId),
                                       Parent ! {self(), response, Response}
                               end),

                           Error = {connect_failed, {ref, {error, nxdomain}}},
                           Child ! {http, {RequestId, {error, Error}}},

                           receive
                               {Child, response, Response} ->
                                   ?assertEqual({error, {http_error, Error}}, Response)
                           after 100 ->
                                   ?assert(timeout)
                           end
                   end),

               it("should return ok when the connection is terminated", fun() ->
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
                                   ?assertEqual({ok, terminate}, Response)
                           after 100 ->
                                   ?assert(timeout)
                           end
                   end)
           end)
   end).
