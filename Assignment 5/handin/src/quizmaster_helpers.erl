-module(quizmaster_helpers).
-export([check_index_in_range/2, get_active_question/1, is_conductor/2, check_if_player_exists/2, check_guess/3, init_distribution/2]).

% check index of guess
check_index_in_range(Index, _) when Index < 1 -> false;
check_index_in_range(Index, Data) when Index > 0 -> index_is_in_range(Index, get_active_question(Data)).

index_is_in_range(Index, {_, Answers}) ->
  Index =< length(Answers).

% get the at the moment active question
get_active_question(Data) ->
  lists:nth(maps:get(active_question, Data), maps:get(questions, Data)).

% check if pid in from is the same as in Data
is_conductor({Pid, _}, Data) ->
  Pid == maps:get(conductor, Data).

% check if player exists in players map
check_if_player_exists(_, []) -> false;
check_if_player_exists(Nickname, [{Playername, _, _} | Players]) ->
  case Nickname == Playername of
    true -> true;
    false -> check_if_player_exists(Nickname, Players)
  end.

% check if guess is correct or not
check_guess(Ref, Index, Data) ->
  CurrentQuestion = get_active_question(Data),
  case is_first_guess(Ref, Data) of
    true -> UpdatedData = maps:update(answered, lists:append([Ref], maps:get(answered, Data)), Data),
          case is_correct(Index, CurrentQuestion) of
              true -> NewData = update_players_score(Ref, UpdatedData, correct), NewData2 = update_distribution(Index, NewData), NewData2;
              false -> NewData = update_players_score(Ref, UpdatedData, incorrect), NewData
            end;
    false -> Data %send back old Data, so only first guess counts
  end.

% only accept the first guess by remembering Ref of players who answered
is_first_guess(Ref, Data) ->
  case lists:member(Ref, maps:get(answered, Data)) of
    true -> false;
    false -> true
  end.

% check if index is the right answer
is_correct(Index, {_, Answers}) ->
  Answer = lists:nth(Index, Answers),
  case Answer of
    {correct, _} -> true;
    _ -> false
  end.

update_players_score(Ref, UpdatedData, Correct) ->
  case Correct of
    correct ->
      {_Nickname, _Pid, Total, _LastScore} = maps:get(Ref, maps:get(players, UpdatedData)),
      maps:update(players, maps:update(Ref, {_Nickname, _Pid, Total+1, 1}, maps:get(players, UpdatedData)), UpdatedData);
    incorrect ->
      {_Nickname, _Pid, Total, _LastScore} = maps:get(Ref, maps:get(players, UpdatedData)),
      maps:update(players, maps:update(Ref, {_Nickname, _Pid, Total, 0}, maps:get(players, UpdatedData)), UpdatedData)
  end.

init_distribution(1, Map) -> Map#{1 => 0};
init_distribution(AnswerIndex, Map) -> init_distribution(AnswerIndex-1, Map#{AnswerIndex => 0}).

% update distribution between index and how many times answered
update_distribution(Index, NewData) ->
  Dist = maps:get(distribution, NewData),
  Count = maps:get(Index, Dist),
  UpdatedDist = Dist#{Index => Count + 1},
  NewData#{distribution => UpdatedDist}.