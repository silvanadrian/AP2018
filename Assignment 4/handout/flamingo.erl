-module(flamingo).

-export([new/1, request/4, route/4, drop_group/2]).

new(Global) ->
     try ({ok, spawn(fun() -> loop({#{}}) end)})
     catch
        _:Error -> {error, Error}
     end.

request(Flamingo, Request, From, Ref) ->
    Flamingo ! {From, Request, Ref}.

route(_Flamingo, _Path, _Fun, _Arg) ->
    nope.

drop_group(_Flamingo, _Id) ->
    not_implemented.

loop(Requests) ->
    receive
        {From, Request, Ref} ->
            From ! {Ref, Request},
            loop(Requests)
    end.