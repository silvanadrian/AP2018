-module(mood).
-export([server/0,try_it_mood/1,try_it_moo/1]).

mood({Path,_}, _, State) ->
  case Path of
    "/mood" ->
      case State of
        1 -> {no_change, "Happy!"};
        _ -> {no_change, "Sad"}
      end;
    "/moo" ->
      {new_state, "That's funny", 1}
  end.

server() ->
  {ok, F} = flamingo:new("Mood Module"),
  flamingo:route(F, ["/mood", "/moo"], fun mood/3, 0),
  F.

try_it_mood(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/mood", []}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  after 5000 ->
    erlang:error(timeout)
  end.

try_it_moo(Server) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/moo", []}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  after 5000 ->
    erlang:error(timeout)
  end.
