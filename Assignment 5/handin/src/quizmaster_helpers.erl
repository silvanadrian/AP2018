-module(quizmaster_helpers).
-export([check_index_in_range/2, get_active_question/1,is_conductor/2]).


check_index_in_range(Index, _) when Index < 1 -> false;
check_index_in_range(Index, Data) when Index > 0 -> index_is_in_range(Index, get_active_question(Data)).

index_is_in_range(Index, {_, Answers}) ->
  Index =< length(Answers).

get_active_question(Data) ->
  lists:nth(maps:get(active_question, Data), maps:get(questions, Data)).

is_conductor({Pid, _}, Data) ->
  Pid == maps:get(conductor, Data).