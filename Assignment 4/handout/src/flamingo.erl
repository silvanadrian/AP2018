-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2]).

new(Global) ->
  try ({ok, spawn(fun() -> loop(Global, #{}) end)})
  catch
    _:Error -> {error, Error}
  end.

request(Flamingo, Request, From, Ref) ->
  Flamingo ! {From, request, Request, Ref}.

route(Flamingo, Path, Fun, Arg) ->
  Flamingo ! {self(), routes, Path, Fun, Arg},
  receive
    Flamingo -> {ok, make_ref()}
  end.


drop_group(_Flamingo, _Id) ->
  not_implemented.

loop(Global, Routes) ->
  receive
    {From, request, {Path, Request}, Ref} ->
      case maps:is_key(Path, Routes) of
        true -> F = maps:get(Path, Routes),
          Content  = F({Path, Request}, Global, Ref),
          From ! {Ref, {200, Content}}
      end,
      loop(Global, Routes);
    {From, routes, Path, Fun, Arg} ->
      L = [{X, Fun} || X <- Path],
      NewRoutes = maps:merge(Routes, maps:from_list(L)),
      From ! self(),
      loop(Global, NewRoutes)
  end.