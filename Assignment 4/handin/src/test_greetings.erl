-module(test_greetings).
-include_lib("eunit/include/eunit.hrl").
-import(greetings, [server/0]).

send_name_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/hello", [{"name", "Student"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Greetings Student\nYou have reached The Flamingo Server"}}, Msg)
  end.

send_name2_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/hello", [{"name", "Obama"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Greetings Obama\nYou have reached The Flamingo Server"}}, Msg)
  end.

send_name_empty_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/hello", [{"name", ""}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Greetings \nYou have reached The Flamingo Server"}}, Msg)
  end.

make_references() ->
  {make_ref(),self()}.