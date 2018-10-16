-module(quizmaster_client).
-export([add_questions/0]).

add_questions() ->
  {ok, Q} = quizmaster:start(),
  quizmaster:add_question(Q, {"Answer to the Ultimate Question of Life, the Universe, and Everything?", ["43", "34", {correct, "42"}, "24"]}),
  quizmaster:add_question(Q, {"How many i's are in Ken's name?", [{correct, "2"}, "10", {correct, "0"}, "6"]}),
  quizmaster:add_question(Q, {"How many legs does a Flamingo have?", ["1", "2", {correct, "0"}, "4"]}),
  quizmaster:get_questions(Q),
  quizmaster:join(Q, "Nick"), % won't bea able to join
  quizmaster:play(Q),
  {ok, Ref} = quizmaster:join(Q, "Nick").

