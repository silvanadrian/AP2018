-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2, matchPrefix/2, getMatchingRoute/3]).

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
loop(Global, RouteGroups) ->
  receive
    {From, request, {Path, Request}, Ref} ->
      MatchedRoute = getMatchingRoute(Path, RouteGroups, ""),
      case MatchedRoute of
        "" -> From ! {Ref, {404, "No matching route found"}};
        _ -> From ! {Ref, {200, getContent(MatchedRoute, Request, RouteGroups)}}
      end,
      loop(Global, RouteGroups);
    {From, routes, Path, Fun, Arg} ->
      NewRoutes = updateRouteGroups(Path, Fun, Arg, RouteGroups),
      From ! NewRoutes,
      loop(Global, NewRoutes)
  end.


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

getMatchingRoute(_Path, [], MatchedRoute) -> MatchedRoute;
getMatchingRoute(Path, [Routes | RestRoutingGroup ], MatchedRoute) ->
    MatchedRouteNew = matchPrefix(Path, Routes),
    case length(MatchedRouteNew) > length(MatchedRoute) of
      true -> getMatchingRoute(Path, RestRoutingGroup, MatchedRouteNew);
      false -> getMatchingRoute(Path, RestRoutingGroup, MatchedRoute)
    end.

matchPrefix(Path, {Routes, Fun, Arg}) ->
  MatchedPrefixes = lists:filter(fun(Route) -> string:left(Path, length(Route)) == Route end,Routes),
  case MatchedPrefixes of
    [] -> "";
    _ -> lists:max(MatchedPrefixes)
  end.

getContent(MatchedRoute, Request, [], A) ->   A(Request).
getContent(MatchedRoute, Request, [Group | RouteGroup], _) ->
  A = matchMatchedRoute(MatchedRoute, Group),
  getContent(MatchedRoute, Request, RouteGroup, A).
