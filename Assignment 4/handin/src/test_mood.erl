-module(test_mood).
-include_lib("eunit/include/eunit.hrl").
-import(mood, [server/0]).

request_mood_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/mood", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Sad"}}, Msg)
  end.

request_moo_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/moo", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "That's funny"}}, Msg)
  end.

request_moo_and_mood_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/moo", []}, Me, Ref),
  receive
    Msg1 -> ?assertEqual({Ref, {200, "That's funny"}}, Msg1)
  end,
  flamingo:request(Server, {"/mood", []}, Me, Ref),
  receive
    Msg2 -> ?assertEqual({Ref, {200, "Happy!"}}, Msg2)
  end.

make_references() ->
  {make_ref(),self()}.