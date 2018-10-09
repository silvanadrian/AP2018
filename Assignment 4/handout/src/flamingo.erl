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
    Flamingo2 -> {ok, Flamingo2}
  end.


drop_group(_Flamingo, _Id) ->
  not_implemented.

% RouteGroup = {[Paths], Fun, Arg}
loop(Global, RouteGroup) ->
  receive
    {From, request, {Path, Request}, Ref} ->
      F = getFuns(Path, RouteGroup),


      case maps:is_key(Path, RouteGroup) of
        true -> F = maps:get(Path, RouteGroup),
          Content  = F({Path, Request}, Global, Ref),
          From ! {Ref, {200, Content}}
      end,
      loop(Global, RouteGroup);
    {From, routes, Path, Fun, Arg} ->
      NewRoutes = updateRouteGroups(Path, Fun, Arg, RouteGroup),
      From ! NewRoutes,
      loop(Global, NewRoutes)
  end.

getFuns(Path, RouteGroup) ->
  L = .

getFun(Path, PathGroup) ->
  

% adds the new Path, Action and arguments to the routing group
updateRouteGroups(Path, Fun, Arg, OldGroup) ->
  [{Path, Fun, Arg} | updateOldGroup(Path, OldGroup)].

% removes older routings with the same path as the new one
% generates a new list
updateOldGroup(_, []) -> [];
updateOldGroup(NewPath, [{Path, Fun, Arg} | GroupTail]) ->
  NewPathGroup = updateOldGroupPaths(NewPath, Path),
  % if a group has no path associated anymore we delete the whole group
  case NewPathGroup == [] of
    true -> updateOldGroup(NewPath, GroupTail);
    false -> [{NewPathGroup, Fun, Arg} | updateOldGroup(NewPath, GroupTail)]
  end.

% skips where the old path is the same as in the new path
% generates a new list with all old paths
updateOldGroupPaths(_, []) -> [];
updateOldGroupPaths(NewPath, [OldPath | OldPathTail]) -> 
  case lists:member(OldPath, NewPath) of
    true -> updateOldGroupPaths(NewPath, OldPathTail);
    false -> [OldPath | updateOldGroupPaths(NewPath, OldPathTail)]
  end.
  