-module(quizmaster_tests).
-export([do_you_want_to_play_a_game/0]).





do_you_want_to_play_a_game() ->
  {ok, Q} = quizmaster:start(),

  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}),

  C = quizmaster_conductor:start(),
  quizmaster_conductor:play(C, Q),

  Player1 = quizmaster_player:start(),
  Player2 = quizmaster_player:start(),

  quizmaster_player:join(Player1, Q, "Jigsaw"),
  quizmaster_player:join(Player2, Q, "Jason Voorhees").
