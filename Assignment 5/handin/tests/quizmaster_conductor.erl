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
    Msg -> {conductor, Msg}
  end.

loop(Pid) ->
  receive
    {play, Quiz} ->
      Pid ! quizmaster:play(Quiz),
      loop(Pid);
    {next, Quiz} ->
      Pid ! quizmaster:next(Quiz),
      loop(Pid);
    {timesup, Quiz} ->
      Pid ! quizmaster:timesup(Quiz),
      loop(Pid);
    Msg ->
      Pid ! Msg,
      loop(Pid)
  end.