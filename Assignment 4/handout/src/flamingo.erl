-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2]).

new(Global) ->
  try ({ok, spawn(fun() -> loop(Global, []) end)})
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

loop(Global, RouteGroup) ->
  receive
    {From, request, {Path, Request}, Ref} ->
      case maps:is_key(Path, RouteGroup) of
        true -> F = maps:get(Path, RouteGroup),
          Content  = F({Path, Request}, Global, Ref),
          From ! {Ref, {200, Content}}
      end,
      loop(Global, RouteGroup);
    {From, routes, Path, Fun, Arg} ->
      NewRoutes = updateRouteGroups(Path, Fun, Arg, RouteGroup),
      From ! self(),
      loop(Global, NewRoutes)
  end.

% RouteGroup = {[Paths], Fun, Arg}

updateRouteGroups(Path, Fun, Arg, OldGroup) ->
  [{Path, Fun, Arg} | updateOldGroup(Path, OldGroup)].

updateOldGroup(NewPath, []) -> [];
updateOldGroup(NewPath, [{Path, Fun, Arg} | GroupTail]) ->
  [{updateOldGroupPaths(NewPath, Path), Fun, Arg} | updateOldGroup(NewPath, GroupTail)].

updateOldGroupPaths(NewPath, []) -> [];
updateOldGroupPaths(NewPath, [OldPath | OldPathTail]) -> 
  case lists:member(OldPath, NewPath) of
    true -> updateOldGroupPaths(NewPath, OldPathTail);
    false -> [OldPath | updateOldGroupPaths(NewPath, OldPathTail)]
  end.
  