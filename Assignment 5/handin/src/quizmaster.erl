-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1,
         join/2, leave/2, guess/3]).
