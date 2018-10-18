-module(quizmaster_player).

-export([start/1, join/3, leave/2, guess/3, timesup/2, next/2]).
%% API

start(Q) -> spawn(fun() -> loop({Q, void, void}) end).

join(Pid, Quiz, Name) ->
  request_reply(Pid, {join, Name, Quiz}).

leave(Pid, Quiz) ->
  request_reply(Pid, {leave, Quiz}).

guess(Pid, Quiz, Index) ->
  request_reply(Pid, {guess, Index, Quiz}).

timesup(Pid, Quiz) ->
  request_reply(Pid, {timesup, Quiz}).

next(Pid, Quiz) ->
  request_reply(Pid, {next, Quiz}).


%% Internal implementation

request_reply(Pid, Request) ->
  Pid ! {Pid, Request},
  receive
    Msg -> Msg
  end.

loop({Pid, Ref, Name}) ->
  receive
    {From, timesup, Quiz} ->
      From ! {player, quizmaster:timesup(Quiz)},
      loop({Pid, Ref, Name});
    {From, next, Quiz} ->
      From ! {player, quizmaster:next(Quiz)},
      loop({Ref, Name});
    {From, {join, NameN, Quiz}} ->
      From ! {player, quizmaster:join(Quiz, NameN)},
      loop({Pid, Ref, Name});
    {From, leave, Quiz} ->
      From ! {player, quizmaster:leave(Quiz, Ref)},
      loop({Pid,Ref, Name});
    {From, guess, Index, Quiz} ->
      From ! {player, quizmaster:guess(Quiz, Ref, Index)},
      loop({Pid, Ref, Name});
    Msg ->
      Pid ! Msg,
      loop({Pid, Ref, Name})
  end.