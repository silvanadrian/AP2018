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
  ?assertMatch({player, {ok, _}}, quizmaster_player:join(Player1, Q, "jigsaw")),
  ?assertEqual({player, {error,is_taken}},quizmaster_player:join(Player1, Q, "jigsaw")),
  Player2 = quizmaster_player:start(self()),
  ?assertMatch({player, {ok,_}}, quizmaster_player:join(Player2, Q, "Jason Voorhees")).

leave_player_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),

  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C, Q),
  Player1 = quizmaster_player:start(self()),
  {player, {ok, Ref}} = quizmaster_player:join(Player1, Q, "jigsaw"),
  ?assertMatch({player, ok},quizmaster_player:leave(Player1, Q, Ref)),
  ?assertEqual({player, {error, who_are_you}},quizmaster_player:leave(Player1, Q, make_ref())).

next_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),

  C = quizmaster_conductor:start(self()),
  ?assertMatch({conductor, ok}, quizmaster_conductor:play(C, Q)),
  Player1 = quizmaster_player:start(self()),
  ?assertMatch({player, {ok,_}},quizmaster_player:join(Player1, Q, "jigsaw")),
  Player2 = quizmaster_player:start(self()),
  ?assertMatch({player, {ok,_}}, quizmaster_player:join(Player2, Q, "Jason Voorhees")),
  ?assertMatch({conductor, {ok, {_, [_ | _]}}}, quizmaster_conductor:next(C, Q)).

guess_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),
  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C, Q),
  quizmaster_conductor:next(C, Q),
  Player1 = quizmaster_player:start(self()),
  {player, {ok, Ref}} = quizmaster_player:join(Player1, Q, "jigsaw"),
  quizmaster_player:guess(Player1, Q, 1, Ref),
  quizmaster_player:guess(Player1, Q, 2, Ref), % second guess get ignored
  quizmaster_player:guess(Player1, Q, 1, make_ref()), % not a joined Player ref, so gets ignored
  ?assertMatch({conductor,{ok,[1,0,0,0], _, _, false}},quizmaster_conductor:timesup(C, Q)),
  quizmaster_conductor:next(C, Q),
  quizmaster_player:guess(Player1, Q, 1, Ref), % correct answer
  ?assertMatch({conductor,{ok,[1,0,0,0], [{"jigsaw", 1}], [{"jigsaw", 1}], false}},quizmaster_conductor:timesup(C, Q)).

timesup_error_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),
  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C,Q),
  ?assertEqual({conductor,{error, no_question_asked}}, quizmaster_conductor:timesup(C,Q)),
  C2 = quizmaster_conductor:start(self()),
  ?assertEqual({conductor,{error, nice_try}}, quizmaster_conductor:timesup(C2,Q)).

do_you_want_to_play_a_game_test() ->
  {ok, Q} = quizmaster:start(),
  quizmaster_add_example_questions(Q),
  C = quizmaster_conductor:start(self()),
  quizmaster_conductor:play(C, Q),
  quizmaster_conductor:next(C, Q), % get first question
  %active_question

  % creating players
  Player1 = quizmaster_player:start(self()),
  Player2 = quizmaster_player:start(self()),
  Player3 = quizmaster_player:start(self()),

  % join all Players
  {player, {ok, Ref1}} = quizmaster_player:join(Player1, Q, "jigsaw"),
  {player, {ok, Ref2}} = quizmaster_player:join(Player2, Q, "Jason Voorhees"),
  {player, {ok, Ref3}} = quizmaster_player:join(Player3, Q, "Pennywise"),

  %guesses
  quizmaster_player:guess(Player1, Q, 1, Ref1),
  quizmaster_player:guess(Player2, Q, 2, Ref2),
  quizmaster_player:guess(Player3, Q, 3, Ref3),

  %timesup first game over
  ?assertEqual({conductor,{ok,[1,1,1,0],
    [{"Jason Voorhees",0},{"Pennywise",1},{"jigsaw", 0}],
    [{"Jason Voorhees",0},{"Pennywise",1},{"jigsaw", 0}], false}},quizmaster_conductor:timesup(C, Q)),
  %between_questions
  % seems Jason has enough
  quizmaster_player:leave(Player2, Q, Ref2),
  % next questions
  quizmaster_conductor:next(C, Q),
  %active_question

  quizmaster_player:guess(Player1, Q, 1, Ref1),
  quizmaster_player:guess(Player2, Q, 2, Ref2), %seems someone guesses for jason -> ignored
  quizmaster_player:guess(Player3, Q, 3, Ref3),
  quizmaster_player:guess(Player3, Q, 1, Ref3), %only first get accepted

  % timesup
  ?assertEqual({conductor,{ok,[1,0,1,0],
    [{"Pennywise",1},{"jigsaw", 1}],
    [{"Pennywise",2},{"jigsaw", 1}], false}},quizmaster_conductor:timesup(C, Q)),

  %between_questions
  % seems a new player joins
  Player4 = quizmaster_player:start(self()),
  {player, {ok, Ref4}} = quizmaster_player:join(Player4, Q, "Voldemort"),

  % next question
  quizmaster_conductor:next(C, Q),

  % active_question
  quizmaster_player:guess(Player1, Q, 1, Ref1),
  quizmaster_player:guess(Player3, Q, 3, Ref3),
  quizmaster_player:guess(Player4, Q, 3, Ref4),

  ?assertEqual({conductor,{ok,[1,0,2,0],
    [{"Pennywise",1},{"Voldemort",1},{"jigsaw",0}],
    [{"Pennywise",3},{"Voldemort",1},{"jigsaw",1}], true}},quizmaster_conductor:timesup(C, Q)).

quizmaster_server() ->
  quizmaster:start().

quizmaster_add_example_questions(Q) ->
  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}).
