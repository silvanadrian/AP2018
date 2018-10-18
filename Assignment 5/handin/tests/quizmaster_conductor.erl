-module(quizmaster_conductor).

-export([start/0, play/2, next/2, timesup/2]).
%% API

start() -> spawn(fun() -> loop() end).

play(Pid, Quiz) ->
  request_reply(Pid, {play, Quiz}).

next(Pid, Quiz) ->
  request_reply(Pid, {next, Quiz}).

timesup(Pid, Quiz) ->
  request_reply(Pid, {timesup, Quiz}).


%% Internal implementation

request_reply(Pid, Request) ->
  Pid ! Request.

loop() ->
  receive
    {play, Quiz} ->
      case quizmaster:play(Quiz) of
        ok ->
          io:fwrite("conductor pressed play~n"),
          loop();
        {error, no_questions} ->
          io:fwrite("can't press play without questions"),
          loop()
      end;
    {next, Quiz} ->
      case quizmaster:next(Quiz) of
        {ok, _Q} ->
          loop();
        {error, has_active_question} ->
          io:fwrite("conductor called next during active question~n"),
          loop()
      end;
    {timesup, Quiz} ->
      case quizmaster:timesup(Quiz) of
        {_, A2, A3, A4, Final} ->
          case Final of
            true ->
              io:fwrite("Final Report ->  Distribution: ~p~n, Score Last Quesiton: ~p~n, Total Score: ~p~n", [A2, A3, A4]);
            false ->
              io:fwrite("Report ->  Distribution: ~p~n, Score Last Quesiton: ~p~n, Total Score: ~p~n", [A2, A3, A4]),
              loop()
          end;
        {error, no_question_asked} ->
          io:fwrite("Can't call timesup! No question asked~n"),
          loop()
      end;
    Msg ->
      io:fwrite("conductor received a new message ~p~n", [Msg]),
      loop()
  end.