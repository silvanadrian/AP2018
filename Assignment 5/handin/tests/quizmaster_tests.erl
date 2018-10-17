-module(quizmaster_tests).
-export([do_you_want_to_play_a_game/0]).

do_you_want_to_play_a_game() ->
  {ok, Q} = quizmaster:start(),
  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}),
  quizmaster:get_questions(Q),
  quizmaster:join(Q, "Nick"), % won't be able to join
  quizmaster:play(Q),
  {ok, Ref} = quizmaster:join(Q, "Nick"),
  quizmaster:next(Q),
  quizmaster:guess(Q, Ref, 3),
  quizmaster:guess(Q, Ref, 3),
  quizmaster:timesup(Q),
  quizmaster:next(Q).% return the same since Nick already guessed once