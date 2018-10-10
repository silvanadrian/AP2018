-module(test_hello).
-include_lib("eunit/include/eunit.hrl").
-import(hello, [server/0]).

hello_response_test() ->
  Server = hello:server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Hello my friend"}}, Msg)
  end.

goodbye_response_test() ->
  Server = hello:server(),
  {Ref, Me} = make_references(),
  flamingo:request(Server, {"/goodbye", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Sad to see you go."}}, Msg)
  end.

make_references() ->
  {make_ref(),self()}.