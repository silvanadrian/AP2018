-module(player).

-export([start/0, join/3, leave/2, guess/3, timesup/2, next/2]).
%% API

start() -> spawn(fun() -> loop({void, void}) end).

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
  Pid ! Request.

loop({Ref, Name}) ->
  receive
    {timesup, Quiz} ->
      Response = quizmaster:timesup(Quiz),
      io:fwrite("Player ~p received a message : ~p~n", [Name, Response]),
      loop({Ref, Name});
    {next, Quiz} ->
      Response = quizmaster:next(Quiz),
      io:fwrite("Player ~p received a message : ~p~n", [Name, Response]),
      loop({Ref, Name});
    {join, NameN, Quiz} ->
      case quizmaster:join(Quiz, NameN) of
        {ok, RefN} ->
          loop({RefN, NameN});
        {error, is_taken} ->
          io:fwrite("Name \"~p\" is taken!~n", [NameN]),
          loop({Ref, Name})
      end;
    {leave, Quiz} ->
      case quizmaster:leave(Quiz, Ref) of
        ok ->
          io:fwrite("Player ~p left from game!~n", [Name]);
        {error, who_are_you} ->
          io:fwrite("Player ~p can't leave the game!~n", [Name]),
          loop({Ref, Name})
      end;
    {guess, Index, Quiz} ->
      _ = quizmaster:guess(Quiz, Ref, Index),
      loop({Ref, Name});
    Msg ->
      io:fwrite("Player ~p received a message : ~p~n", [Name, Msg]),
      loop({Ref, Name})
  end.