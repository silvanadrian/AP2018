-module(quizmaster_conductor).

-export([start/1, play/2, next/2, timesup/2]).

start(Q) -> spawn(fun() -> loop(Q) end).

play(Pid, Quiz) ->
  request_reply(Pid, {play, Quiz}).

next(Pid, Quiz) ->
  request_reply(Pid, {next, Quiz}).

timesup(Pid, Quiz) ->
  request_reply(Pid, {timesup, Quiz}).

request_reply(Pid, Request) ->
  Pid ! Request,
  receive
    Msg -> Msg
  end.

loop(Pid) ->
  receive
    {play, Quiz} ->
      Pid ! {conductor, quizmaster:play(Quiz)},
      loop(Pid);
    {next, Quiz} ->
      Pid ! {conductor, quizmaster:next(Quiz)},
      loop(Pid);
    {timesup, Quiz} ->
      case quizmaster:timesup(Quiz) of
        {OK, Dist, LastQ, Total, Final} ->
          Pid ! {conductor, {OK, Dist, maps:to_list(LastQ), maps:to_list(Total), Final}};
        {error,no_question_asked} ->
          Pid ! {conductor, {error,no_question_asked}};
        {error,nice_try} ->
          Pid ! {conductor, {error,nice_try}}
      end,
      loop(Pid);
    _ ->
      % ignore all other messages for easier testing
      loop(Pid)
  end.