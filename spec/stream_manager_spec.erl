-module(stream_client_manager_spec).
-include_lib("espec.hrl").

spec() ->
     describe("stream manager", fun() ->
        %% meck setup
        before_all(fun() ->
            ok = meck:new(stream_client, [passthrough])
        end),

        after_each(fun() ->
            ?assertEqual(true, meck:validate(stream_client)),
            meck:reset(stream_client)
        end),

        after_all(fun() ->
            ok = meck:unload(stream_client)
        end),

        %% manager setup
        before_each(fun() ->
            {ok, _} = stream_manager:start_link(test_stream_manager),
            ?assertEqual(disconnected, stream_manager:status(test_stream_manager))
        end),

        after_each(fun() ->
            stopped = stream_manager:stop(test_stream_manager)
        end),

        describe("#start_stream", fun() ->
            it("starts streaming", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    % TODO check correct params are passed
                    fun(_, _, _, _) ->
                        Parent ! {self(), started},
                        receive _ -> {ok, terminate} end
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),
                ?assertEqual(connected, stream_manager:status(test_stream_manager)),

                % starting the client happens async, we need to wait for it
                % to return to check it was called (meck thing)
                receive
                    {Child, started} ->
                        Child ! {shutdown}
                after 100 ->
                        ?assert(timeout)
                end,

                meck:wait(stream_client, connect, '_', 100)
            end),

            it("doesn't start a second client if there is one running", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    fun(_, _, _, _) ->
                        Parent ! {self(), started},
                        receive _ -> {ok, terminate} end
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),
                ok = stream_manager:start_stream(test_stream_manager),
                ?assertEqual(connected, stream_manager:status(test_stream_manager)),

                % starting the client happens async, we need to wait for it
                % to return to check it was called (meck thing)
                receive
                    {Child, started} ->
                        Child ! {shutdown}
                after 100 ->
                        ?assert(timeout)
                end,

                meck:wait(stream_client, connect, '_', 100)
            end)
        end),

        describe("client errors", fun() ->
            it("handles unauthorised error", fun() ->
                meck:expect(stream_client, connect,
                    fun(_, _, _, _) -> {error, unauthorised} end
                ),

                stream_manager:start_stream(test_stream_manager),
                meck:wait(stream_client, connect, '_', 100),
                ?assertEqual({error, unauthorised}, stream_manager:status(test_stream_manager))
            end),

            it("handles http errors", fun() ->
                meck:expect(stream_client, connect,
                    fun(_, _, _, _) -> {error, {http_error, something_went_wrong}} end
                ),

                stream_manager:start_stream(test_stream_manager),
                meck:wait(stream_client, connect, '_', 100),
                ?assertEqual({error, {http_error, something_went_wrong}}, stream_manager:status(test_stream_manager))
            end)
        end),

        describe("#stop_stream", fun() ->
            it("shuts down the client", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    % TODO check correct params are passed
                    fun(_, _, _, _) ->
                        Parent ! {self(), started},
                        receive _ -> {ok, terminate} end
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),
                ?assertEqual(connected, stream_manager:status(test_stream_manager)),

                % starting the client happens async, wait for it to start
                % before terminating it
                ChildPid = receive
                               {Child, started} ->
                                   Child
                           after 100 ->
                                   ?assert(timeout)
                           end,

                ok = stream_manager:stop_stream(test_stream_manager),

                % wait for child process to end
                meck:wait(stream_client, connect, '_', 100),

                ?assertEqual(disconnected, stream_manager:status(test_stream_manager)),

                % check the child process is no longer alive
                ?assertEqual(is_process_alive(ChildPid), false)
            end)
        end),

        describe("#set_params", fun() ->
            it("sets the params to track", fun() ->
                Params = "params=true",

                meck:expect(stream_client, connect,
                    fun(_, _, _, _) -> {ok, terminate} end
                ),

                stream_manager:set_params(test_stream_manager, Params),
                stream_manager:start_stream(test_stream_manager),

                meck:wait(stream_client, connect, ['_', '_', Params, '_'], 100)
            end),

            it("restarts the client if connected", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    fun(_, _, _, _) ->
                        Parent ! {self(), started},
                        receive _ -> {ok, terminate} end
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),

                % wait for child 1 to start, we only need this to get the pid
                % at this point
                Child1 = receive
                             {Child1Pid, started} ->
                                 Child1Pid
                         after 100 ->
                             ?assert(timeout)
                         end,

                NewParams = "params=true",
                stream_manager:set_params(test_stream_manager, NewParams),

                % child 1 will be terminated by the manager, and this call will
                % return so we can wait for it through meck
                meck:wait(stream_client, connect, ['_', '_', "", '_'], 100),

                % starting the client happens async, we need to wait for it
                % to return to check it was called (meck thing)
                Child2 = receive
                            {Child2Pid, started} ->
                                Child2Pid ! {shutdown}
                        after 100 ->
                                ?assert(timeout)
                        end,

                meck:wait(stream_client, connect, ['_', '_', NewParams, '_'], 100),

                % check two seperate processes were started
                ?assertNotEqual(Child1, Child2)
            end)
        end),

        describe("#set_auth", fun() ->
            it("sets the auth", fun() ->
                Auth = {basic, ["User1", "Pass1"]},

                meck:expect(stream_client, connect,
                    fun(_, _, _, _) -> {ok, terminate} end
                ),

                stream_manager:set_auth(test_stream_manager, Auth),
                stream_manager:start_stream(test_stream_manager),

                meck:wait(stream_client, connect, ['_', Auth, '_', '_'], 100)
            end),

            it("restarts the client if connected", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    fun(_, _, _, _) ->
                        Parent ! {self(), started},
                        receive _ -> {ok, terminate} end
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),

                % wait for child 1 to start, we only need this to get the pid
                % at this point
                Child1 = receive
                             {Child1Pid, started} ->
                                 Child1Pid
                         after 100 ->
                             ?assert(timeout)
                         end,

                NewAuth = {basic, ["User2", "Pass2"]},
                stream_manager:set_auth(test_stream_manager, NewAuth),

                % child 1 will be terminated by the manager, and this call will
                % return so we can wait for it through meck
                OldAuth = {basic, ["", ""]},
                meck:wait(stream_client, connect, ['_', OldAuth, '_', '_'], 100),

                % starting the client happens async, we need to wait for it
                % to return to check it was called (meck thing)
                Child2 = receive
                            {Child2Pid, started} ->
                                Child2Pid ! {shutdown}
                        after 100 ->
                                ?assert(timeout)
                        end,

                meck:wait(stream_client, connect, ['_', NewAuth, '_', '_'], 100),

                % check two seperate processes were started
                ?assertNotEqual(Child1, Child2)
            end)
        end),

        describe("#set_callback", fun() ->
            it("sets the callback to call with data", fun() ->
                Parent = self(),

                HandleConnection = fun(Self, Callback) ->
                        receive
                            {data, Data} ->
                                Callback(Data),
                                Parent ! {self(), callback},
                                Self(Self, Callback);
                            _ ->
                                {ok, terminate}
                        end
                end,

                meck:expect(stream_client, connect,
                    fun(_, _, _, Callback) ->
                        Parent ! {self(), started},
                        HandleConnection(HandleConnection, Callback)
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),

                % wait for child to start
                Child = receive
                             {ChildPid, started} ->
                                 ChildPid
                         after 100 ->
                             ?assert(timeout)
                         end,

                % send some data
                Child ! {data, data1},

                % wait for callback to be called
                receive
                    {Child, callback} ->
                        ok
                after 100 ->
                    ?assert(timeout)
                end,

                Callback1 = fun(Data) ->
                    Parent ! {callback1, Data}
                end,

                Callback2 = fun(Data) ->
                    Parent ! {callback2, Data}
                end,

                % set a callback
                stream_manager:set_callback(test_stream_manager, Callback1),

                % send some more data
                Child ! {data, data2},

                % wait for callback to be called
                receive
                    {Child, callback} ->
                        ok
                after 100 ->
                    ?assert(timeout)
                end,

                % set another callback
                stream_manager:set_callback(test_stream_manager, Callback2),

                % send some more data
                Child ! {data, data3},

                % wait for callback to be called
                receive
                    {Child, callback} ->
                        ok
                after 100 ->
                    ?assert(timeout)
                end,

                % check callbacks called correctly
                receive
                    {callback1, data2} ->
                        ok
                after 100 ->
                    ?assert(timeout)
                end,

                receive
                    {callback2, data3} ->
                        ok
                after 100 ->
                    ?assert(timeout)
                end
            end)
        end)
    end).
