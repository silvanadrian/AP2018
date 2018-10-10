-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2, matchPrefix/2, getMatchingRoute/3]).

new(Global) ->
  try ({ok, spawn(fun() -> loop(Global, []) end)})
  catch
    _:Error -> {error, Error}
  end.

request(Flamingo, Request, From, Ref) ->
  Flamingo ! {From, request, Request, Ref}.

% add error !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
route(Flamingo, Path, Fun, Arg) ->
  Flamingo ! {self(), routes, Path, Fun, Arg},
  receive
    {Status, Content} -> {Status, Content}
  end.

drop_group(_Flamingo, _Id) ->
  not_implemented.

loop(Global, RouteGroups) ->
  receive
    % requests
    {From, request, {Path, Request}, Ref} ->
      % get the matching route, with action(Fun) and state(Arg)
      {MatchedRoute, Fun, Arg} = getMatchingRoute(Path, RouteGroups, {"", none, none}),
      case MatchedRoute of
        "" -> From ! {Ref, {404, "No matching route found"}};
        _ -> 
        % try to run the action
        try Fun({MatchedRoute, Request}, Global, Arg) of
          R -> 
            case R of
              % if the action returns new_state we update the local state
              {new_state, Content, NewState} ->
                NewRouteGroups = updateState(RouteGroups, MatchedRoute, NewState),
                From ! {Ref, {200, Content}}, % everything worked fine so 200
                loop(Global, NewRouteGroups);
              {no_change, Content} ->
                From ! {Ref, {200, Content}}, % everything worked fine so 200
                loop(Global, RouteGroups)
            end
        catch
          error:_ ->
            From ! {Ref, {500, "error in action"}},
            loop(Global, RouteGroups)
        end
      end,
      loop(Global, RouteGroups);
    % new routes, need to update routing groups
    {From, routes, Path, Fun, Arg} ->
      try NewRoutes = updateRouteGroups(Path, Fun, Arg, RouteGroups) of
        _ -> From ! {ok, self()}
        catch
          _:Reason -> {error, Reason}
      end,
      loop(Global, NewRoutes)
  end.

% update local state if an action returned a state
updateState([{Path, Fun, Arg} | GroupTail], MatchedRoute, NewState) ->
  case lists:member(MatchedRoute, Path) of
    true -> [{Path, Fun, NewState} | GroupTail];
    false -> [{Path, Fun, Arg} | updateState(GroupTail, MatchedRoute, NewState)]
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

% gets the matching route by calling for the prefix
% and if it is longer than the one we had from before it updates
getMatchingRoute(_Path, [], MatchedRoute) -> MatchedRoute;
getMatchingRoute(Path, [Routes | RestRoutingGroup ], {MatchedRoute, Fun, Arg}) ->
    {MatchedRouteNew, FunNew, ArgNew} = matchPrefix(Path, Routes),
    case length(MatchedRouteNew) > length(MatchedRoute) of
      true -> getMatchingRoute(Path, RestRoutingGroup, {MatchedRouteNew, FunNew, ArgNew});
      false -> getMatchingRoute(Path, RestRoutingGroup, {MatchedRoute, Fun, Arg})
    end.

% gets the longest prefix from this routing group
% if no prefixes it returns the empty string
matchPrefix(Path, {Routes, Fun, Arg}) ->
  MatchedPrefixes = lists:filter(fun(Route) -> string:left(Path, length(Route)) == Route end,Routes),
  case MatchedPrefixes of
    [] -> {"", Fun, Arg};
    _ -> {lists:max(MatchedPrefixes), Fun, Arg}
  end.
