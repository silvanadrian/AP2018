-module(counter).
-export([server/0,try_it_inc/2,try_it_dec/2]).

getNum(Arg) ->
  % check if x is in one of the tuples
  case lists:keyfind("x", 1, Arg) of
    false -> 1;
    {"x", SNumber} -> 
      case string:to_integer(SNumber) of
        {error, _} -> 1;
        {Int, _} -> 
          case Int > 0 of
            false -> 1;
            true -> Int
          end
      end
  end.

counter({Path,Arg}, _, State) ->
  case Path of
    "/inc_with" ->
      NewState = State + getNum(Arg),
      Content = integer_to_list(NewState),
      {new_state, Content, NewState};
    "/dec_with" ->
      NewState = State - getNum(Arg),
      Content = integer_to_list(NewState),
      {new_state, Content, NewState}
  end.

server() ->
  {ok, F} = flamingo:new("Mood Module"),
  flamingo:route(F, ["/inc_with", "/dec_with"], fun counter/3, 0),
  F.

try_it_inc(Server, Arg) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/inc_with", Arg}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  after 5000 ->
    erlang:error(timeout)
  end.

try_it_dec(Server, Arg) ->
  Me = self(),
  Ref = make_ref(),
  flamingo:request(Server, {"/dec_with", Arg}, Me, Ref),
  receive
    {Ref, Reply} -> Reply
  after 5000 ->
    erlang:error(timeout)
  end.
