-module(test_counter).
-include_lib("eunit/include/eunit.hrl").
-import(counter, [server/0]).

inc_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/inc_with", [{"x","2"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "2"}}, Msg)
  end.

dec_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/dec_with", [{"x","2"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "-2"}}, Msg)
  end.

inc_empty_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/inc_with", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "1"}}, Msg)
  end.

dec_empty_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/dec_with", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "-1"}}, Msg)
  end.

mixture_dec_inc_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/dec_with", [{"x","2"}]}, Me, Ref),
  receive
    Msg1 -> ?assertEqual({Ref, {200, "-2"}}, Msg1)
  end,
  flamingo:request(Server, {"/inc_with", [{"x","1"}]}, Me, Ref),
  receive
    Msg2 -> ?assertEqual({Ref, {200, "-1"}}, Msg2)
  end,
  flamingo:request(Server, {"/inc_with", [{"x","1"}]}, Me, Ref),
  receive
    Msg3 -> ?assertEqual({Ref, {200, "0"}}, Msg3)
  end.

negative_number_inc_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/inc_with", [{"x","-2"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "1"}}, Msg)
  end.

negative_number_dec_test() ->
  Server = server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/dec_with", [{"x","-2"}]}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "-1"}}, Msg)
  end.

make_references() ->
  {make_ref(),self()}.