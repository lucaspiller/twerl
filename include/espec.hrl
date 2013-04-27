-compile([
    {parse_transform, espec_transform}
  ]).
-export([
    spec/0
  ]).
-import(espec_helper, [spec_set/2, spec_get/1]).
-include_lib("eunit/include/eunit.hrl").
