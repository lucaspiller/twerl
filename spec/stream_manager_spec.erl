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
            {ok, _} = stream_manager:start_link(test_stream_manager)
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
                        Parent ! {self(), started}
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),

                % starting the client happens async
                receive
                    {_, started} ->
                        ok
                after 100 ->
                        ?assert(timeout)
                end,

                [{_, {stream_client, connect, _}, _}] = meck:history(stream_client)
            end),

            it("doesn't start a second client if there is one running", fun() ->
                Parent = self(),

                meck:expect(stream_client, connect,
                    % TODO check correct params are passed
                    fun(_, _, _, _) ->
                        Parent ! {self(), started}
                    end
                ),

                ok = stream_manager:start_stream(test_stream_manager),
                ok = stream_manager:start_stream(test_stream_manager),

                % starting the client happens async
                receive
                    {_, started} ->
                        ok
                after 100 ->
                        ?assert(timeout)
                end,

                [{_, {stream_client, connect, _}, _}] = meck:history(stream_client)
            end)
        end)
    end).
