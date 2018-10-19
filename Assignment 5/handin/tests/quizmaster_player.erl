-module(quizmaster_player).

-export([start/1, join/3, leave/3, guess/4, timesup/2, next/2]).

start(Q) -> spawn(fun() -> loop(Q) end).

join(Pid, Quiz, Name) ->
  request_reply(Pid, {join, Name, Quiz}).

leave(Pid, Quiz, Ref) ->
  request_reply(Pid, {leave, Quiz, Ref}).

guess(Pid, Quiz, Index, Ref) ->
  request_reply(Pid, {guess, Index, Quiz, Ref}).

timesup(Pid, Quiz) ->
  request_reply(Pid, {timesup, Quiz}).

next(Pid, Quiz) ->
  request_reply(Pid, {next, Quiz}).

request_reply(Pid, Request) ->
  Pid ! {Pid, Request},
  receive
    Msg -> Msg
  end.

loop(Pid) ->
  receive
    {From, timesup, Quiz} ->
      From ! {player, quizmaster:timesup(Quiz)},
      loop(Pid);
    {From, next, Quiz} ->
      From ! {player, quizmaster:next(Quiz)},
      loop(Pid);
    {From, {join, Name, Quiz}} ->
      From ! {player, quizmaster:join(Quiz, Name)},
      loop(Pid);
    {From, {leave, Quiz, Ref}} ->
      From ! {player, quizmaster:leave(Quiz, Ref)},
      loop(Pid);
    {From, {guess, Index, Quiz, Ref}} ->
      From ! {player, quizmaster:guess(Quiz, Ref, Index)},
      loop(Pid);
    Msg ->
      % irgnore all other cases for easier testing
      case Msg of
        {player, {ok, _}} -> Pid ! Msg;
        {player, {error, _}} -> Pid ! Msg;
        {player, ok} -> Pid ! Msg;
          _ -> void
      end,
      loop(Pid)
  end.