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
        end)
    end).
