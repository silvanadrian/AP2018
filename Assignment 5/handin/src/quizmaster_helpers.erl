-module(quizmaster_helpers).
-export([check_index_in_range/3,
  get_active_question/1,
  is_conductor/2,
  check_if_player_exists/2,
  check_guess/3,
  init_distribution/2,
  broadcast_next_question/2,
  broadcast_quiz_over/2,
  get_report/2,
  reset_last_points/1,
  reset_points/1
  ]).

% check index of guess
check_index_in_range(Index, _, _) when Index < 1 -> false;
check_index_in_range(Index, Data, Ref) when Index > 0 ->
  index_is_in_range(Index, get_active_question(Data)) and maps:is_key(Ref, maps:get(players, Data)).

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
check_if_player_exists(Nickname, [{Playername, _, _, _} | Players]) ->
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
        false -> NewData = update_players_score(Ref, UpdatedData, incorrect), NewData2 = update_distribution(Index, NewData), NewData2
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
      maps:update(players, maps:update(Ref, {_Nickname, _Pid, Total + 1, 1}, maps:get(players, UpdatedData)), UpdatedData);
    incorrect ->
      {_Nickname, _Pid, Total, _LastScore} = maps:get(Ref, maps:get(players, UpdatedData)),
      maps:update(players, maps:update(Ref, {_Nickname, _Pid, Total, 0}, maps:get(players, UpdatedData)), UpdatedData)
  end.

init_distribution(1, Map) -> Map#{1 => 0};
init_distribution(AnswerIndex, Map) -> init_distribution(AnswerIndex - 1, Map#{AnswerIndex => 0}).

% update distribution between index and how many times answered
update_distribution(Index, NewData) ->
  Dist = maps:get(distribution, NewData),
  Count = maps:get(Index, Dist),
  UpdatedDist = Dist#{Index => Count + 1},
  NewData#{distribution => UpdatedDist}.

% broadcast next_question to all players
broadcast_next_question({_Description, _Answers}, []) -> void;
broadcast_next_question({Description, Answers}, [{Ref, {_, Pid, _, _}} | Players]) ->
  NewAnswers = remove_correct(Answers),
  Pid ! {next_question, Ref, {Description, NewAnswers}},
  broadcast_next_question({Description, Answers}, Players).

% broadcast next_question to all players
broadcast_quiz_over({_,_}, []) -> void;
broadcast_quiz_over({Q,O}, [{_, {_, Pid, _, _}} | Players]) ->
  Pid ! {Q, quiz_over},
  broadcast_quiz_over({Q,O}, Players).

get_report(Data, LastQ) ->
  LastPoints = get_points_last_question(maps:to_list(maps:get(players, Data))),
  TotalPoints = get_points_total(maps:to_list(maps:get(players, Data))),
  {ok, maps:values(maps:get(distribution, Data)), maps:from_list(LastPoints), maps:from_list(TotalPoints), LastQ}.

get_points_last_question([]) -> [];
get_points_last_question([{_Ref, {Name, _Pid, _Total, LastScore}} | T]) ->
  [{Name, LastScore} | get_points_last_question(T)].

get_points_total([]) -> [];
get_points_total([{_Ref, {Name, _Pid, Total, _LastScore}} | T]) ->
  [{Name, Total} | get_points_total(T)].

remove_correct([]) -> [];
remove_correct([Answer | Answers]) ->
  case Answer of
    {_, Text} -> [Text | remove_correct(Answers)];
    Text -> [Text | remove_correct(Answers)]
  end.


reset_last_points(Data) ->
  PlayerList = reset_points(maps:to_list(maps:get(players, Data))),
  maps:update(players, maps:from_list(PlayerList), Data).

reset_points([]) -> [];
reset_points([{Ref, {Nickname, Pid, Total, _}} | Players]) ->
  [{Ref, {Nickname, Pid, Total, 0}} | reset_points(Players)].