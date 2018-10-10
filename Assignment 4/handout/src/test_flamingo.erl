-module(test_flamingo).
-include_lib("eunit/include/eunit.hrl").
-import(flamingo, [new/1,request/4,route/4]).

%% Flamingo Tests
new_server_test() ->
  ?assertMatch({ok, _},server()).

route_test() ->
  {ok, F} = server(),
  ?assertMatch({ok, _Id}, hello_route(F)).

failing_request_test() ->
  {ok, F} = server(),
  hello_route(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/test", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {404, "No matching route found"}}, Msg)
  end.

failing_request_near_match_test() ->
  {ok, F} = server(),
  hello_route(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hella", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {404, "No matching route found"}}, Msg)
  end.

error_request_test() ->
  {ok, F} = server(),
  hello_route(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {500, "error in action"}}, Msg)
  end.

prefix_exact_match_test() ->
  {ok, F} = server(),
  hello_route_function(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Hello, tests are cool"}}, Msg)
  end.


prefix_partial_match_test() ->
  {ok, F} = server(),
  hell_route(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Hello, tests are cool"}}, Msg)
  end.

same_prefix_in_different_groups_test() ->
  {ok, F} = server(),
  hello_route_function(F),
  goodbye_route_function(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Goodbye, tests are cool"}}, Msg)
  end.

same_prefix_in_same_group_test() ->
  {ok, F} = server(),
  hello_route_double_function(F),
  {Ref, Me} = make_references(),
  flamingo:request(F, {"/hello", []}, Me, Ref),
  receive
    Msg -> ?assertEqual({Ref, {200, "Hello, tests are cool"}}, Msg)
  end.

no_prefix_test() ->
  {ok, F} = server(),
  ?assertEqual({error, "No Path given"}, flamingo:route(F, [], x, none)).

empty_prefix_test() ->
  {ok, F} = server(),
  ?assertEqual({error, "Empty Path given"}, flamingo:route(F, [""], x, none)).

% server helper function
server() ->
  flamingo:new("Test").

hello_route(Server) ->
  flamingo:route(Server, ["/hello"], test, none).

hell_route(Server) ->
  flamingo:route(Server, ["/hell"], fun hello/3, none).

make_references() ->
  {make_ref(),self()}.

hello({_, _}, _, _) ->
  {no_change, "Hello, tests are cool"}.

goodbye({_, _}, _, _) ->
  {no_change, "Goodbye, tests are cool"}.

hello_route_function(Server) ->
  flamingo:route(Server, ["/hello"], fun hello/3, none).

goodbye_route_function(Server) ->
  flamingo:route(Server, ["/hello"], fun goodbye/3, none).

hello_route_double_function(Server) ->
  flamingo:route(Server, ["/hello", "/hello"], fun hello/3, none).

%% Hello Module Tests


%% Mood Module Tests