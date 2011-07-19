-module(stream_manager_test).

-include_lib("eunit/include/eunit.hrl").

setup_test_() ->
  {inorder, {foreach, fun setup/0, fun cleanup/1,
      [

      ]}}.

setup() ->
  meck:new(stream_client),
  meck:new(stream_client_util),
  stream_manager:start(test_manager).

cleanup(_) ->
  stream_manager:stop(test_manager),
  meck:unload(stream_client_util),
  meck:unload(stream_client).

ok_test() ->
  ok.
