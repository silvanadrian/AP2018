-module(hello).
-export([server/0,try_it_hello/1,try_it_goodbye/1]).

hello() -> "Hello my friend".
goodbye() -> "Sad to see you go".

server() ->
  F = flamingo:new("Hello Module"),
  flamingo:route(F, ["/hello"], fun hello/0, none),
  flamingo:route(F, ["/goodbye"], fun goodbye/0, none),
  F.

try_it_hello(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/hello", []}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  after 5000 ->
    erlang:error(timeout)
  end.

try_it_goodbye(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/goodbye", []}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  end.
