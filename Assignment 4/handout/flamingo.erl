-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2]).

new(Global) ->
     try ({ok, spawn(fun() -> loop({#{}}) end)})
     catch
        _:Error -> {error, Error}
     end.

request(Flamingo, Request, From, Ref) ->
    Flamingo ! {From, request, Request, Ref}.

route(Flamingo, Path, Fun, Arg) ->
    Flamingo ! {self(), routes, Path, Fun, Arg},
    receive
        {Path, Fun, Arg} -> {ok, 1}
    end.


drop_group(_Flamingo, _Id) ->
    not_implemented.

loop(Requests) ->
    receive
        {From, request, Request, Ref} ->
            From ! {Ref, Request},
            loop(Requests);
         {From, routes, Path, Fun, Arg} ->
             From ! {Path, Fun, Arg},
            loop(Requests)
    end.