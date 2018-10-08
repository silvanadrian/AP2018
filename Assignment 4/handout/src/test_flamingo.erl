-module(test_flamingo).
-include_lib("eunit/include/eunit.hrl").
-import(flamingo, [new/1,request/4,route/4]).

new_server_test() ->
  ?assertMatch({ok, _},flamingo:new("Test")).