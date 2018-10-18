-module(quizmaster_tests).
-include_lib("eunit/include/eunit.hrl").

%%do_you_want_to_play_a_game() ->
%%  {ok, Q} = quizmaster:start(),
%%
%%  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
%%  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
%%  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}),
%%
%%  C = quizmaster_conductor:start(),
%%  quizmaster_conductor:play(C, Q),
%%
%%  Player1 = quizmaster_player:start(),
%%  Player2 = quizmaster_player:start(),
%%
%%  P = quizmaster_player:join(Player1, Q, "Jigsaw"),
%%  %%quizmaster_player:join(Player2, Q, "Jason Voorhees"),
%%  %%timer:sleep(1000),
%%  P.

quizmaster_start_test() ->
  ?assertMatch({ok, _}, quizmaster_server()).

quizmaster_add_question_test() ->
  {ok, Q} = quizmaster_server(),
 ?assertEqual(ok,
  quizmaster:add_question(Q,{"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}]})),
  ?assertMatch({error,_}, quizmaster:add_question(Q, {fail, []})).

quizmaster_get_questions_test() ->
  {ok, Q} = quizmaster_server(),
  ?assertEqual([], quizmaster:get_questions(Q)),
  quizmaster_add_example_questions(Q),
  ?assertMatch([_Question | _Questions], quizmaster:get_questions(Q)).

quizmaster_join_while_editable_test() ->
  {ok, Q} = quizmaster_server(),
  ?assertMatch({error, _}, quizmaster:join(Q, "Hello")).

quizmaster_play_test() ->
  {ok, Q} = quizmaster_server(),
  C = quizmaster_conductor:start(self()),
  ?assertEqual({conductor, {error, no_questions}},quizmaster_conductor:play(C,Q)),
  quizmaster_add_example_questions(Q),
  ?assertMatch({conductor, ok}, quizmaster_conductor:play(C,Q)).

join_player_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),

  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C, Q),

  Player1 = quizmaster_player:start(self()),
  quizmaster_player:join(Player1, Q, "jigsaw"),
  receive
    {player, Msg} -> ?assertMatch({ok, _}, Msg)
  end,
  ?assertEqual({player, {error,is_taken}},quizmaster_player:join(Player1, Q, "jigsaw")),
  Player2 = quizmaster_player:start(self()),
  quizmaster_player:join(Player2, Q, "Jason Voorhees"),
  receive
    {player, Message2} -> ?assertMatch({ok,_}, Message2)
  end.

next_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),

  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C, Q),

  Player1 = quizmaster_player:start(self()),
  quizmaster_player:join(Player1, Q, "jigsaw"),
  receive
    {player, Message} -> ?assertMatch({ok,_}, Message)
  end,
  Player2 = quizmaster_player:start(self()),
  quizmaster_player:join(Player2, Q, "Jason Voorhees"),
  receive
    {player, Message2} -> ?assertMatch({ok,_}, Message2)
  end,
  quizmaster_conductor:next(C, Q).


quizmaster_server() ->
  quizmaster:start().

quizmaster_add_example_questions(Q) ->
  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}).
