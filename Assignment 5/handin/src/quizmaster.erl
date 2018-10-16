-module(quizmaster).
-behaviour(gen_statem).
%% API exports.
-export([start/0, add_question/2, get_questions/1,
  play/1, next/1, timesup/1,
  join/2, leave/2, guess/3]).
%% Gen_statem callbacks
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([editable/3,between_questions/3,active_question/3]).

%%%-------------------------------------------------------------------
%%%
%%% Quizmaster API
%%%
%%%-------------------------------------------------------------------
start() ->
  gen_statem:start({local, quizmaster_server}, ?MODULE, [], []).

add_question(Server, {Description, Answers}) ->
  gen_statem:call(Server, {add_question, {Description, Answers}}).

get_questions(Server) ->
  gen_statem:call(Server, get_questions).

play(Server) ->
  gen_statem:call(Server, play).

next(Server) ->
  gen_statem:call(Server, next).

timesup(Server) ->
  gen_statem:call(Server, timesup).

join(Server, Nickname) ->
  gen_statem:call(Server, {join, Nickname}).

leave(Server, Ref) ->
  gen_statem:call(Server, {leave, Ref}).

guess(Server, Ref, Index) ->
  gen_statem:call(Server, {guess, Ref, Index}).



% get_questions Call
handle_event({call, From}, get_questions, Data) ->
  Questions = maps:get(questions, Data),
  {keep_state, Data , {reply, From, Questions}};

% start playing a quiz -> change state to between_questions
handle_event({call, From}, play, Data) ->
  case Data of
    [] -> {keep_state, Data, {reply, From, {error, no_questions}}};
    _ -> {Pid, _} = From,
      Conductor = maps:update(conductor, Pid, Data),
      {next_state, between_questions, Conductor, {reply, From, ok}}
  end;

handle_event({call, From}, {join, Nickname}, Data) ->
  PlayersValues = maps:values(maps:get(players, Data)),
  case quizmaster_helpers:check_if_player_exists(Nickname, PlayersValues) of
    true -> {keep_state, Data, {reply, From, {error, is_taken}}};
    false ->  {Pid, _} = From,
      PlayersMap = maps:get(players, Data),
      Ref = make_ref(),
      NewData = Data#{players => PlayersMap#{Ref => {Nickname, Pid, 0, 0}}},
      maps:get(conductor, NewData) ! {player_joined, Nickname, maps:size(maps:get(players, NewData))},
      {keep_state, NewData, {reply, From, {ok, Ref}}}
  end;

handle_event({call, From}, {leave, Ref}, Data) ->
  case maps:is_key(Ref, maps:get(players, Data)) of
    true ->
      {Nickname, _, _} = maps:get(Ref, maps:get(players, Data)),
      UpdatedPlayers = maps:remove(Ref, maps:get(players, Data)),
      NewData = maps:update(players, UpdatedPlayers, Data),
      maps:get(conductor, NewData) ! {player_left, Nickname, maps:size(maps:get(players, NewData))},
      {keep_state, NewData, {reply, From, ok}};
    false ->  {keep_state, Data, {reply, From, {error, who_are_you}}}
  end;

% ignore all other unhandled events
handle_event({_,From},_,Data) ->
  {keep_state, Data, {reply, From, {error, "Unhandled event"}}}.

editable({call, From}, {add_question, Question}, Data) ->
  case Question of
    {_,[_|_]} ->   OldQuestions = maps:get(questions, Data),
      UpdatedQuestions = maps:update(questions, lists:append(OldQuestions, [Question]), Data),
      {keep_state, UpdatedQuestions , {reply, From, ok}};
    {_, []} -> {keep_state, Data , {reply, From, {error, "Question is in wrong format"}}}
  end;

% catch join message while editable
editable({call,From}, {join, _Name}, Data) ->
  {keep_state, Data, {reply, From, {error, "Can't join while editable"}}};

editable(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

between_questions({call, From}, next, Data) ->
  case quizmaster_helpers:is_conductor(From, Data) of
    true -> Question = lists:nth(maps:get(active_question, Data), maps:get(questions, Data)),
            {next_state, active_question, Data, {reply, From, {ok, Question}}};
    false -> {keep_state, Data, {reply, From, {error, who_are_you}}}
  end;

between_questions({call, From}, timesup, Data) ->
  case quizmaster_helpers:is_conductor(From, Data) of
    true -> {keep_state, Data, {reply, From, {error, no_question_asked}}};
    false -> {keep_state, Data, {reply, From, {error, nice_try}}}
  end;
between_questions({call, From}, {join, Name}, Data) ->
  handle_event({call, From}, {join, Name}, Data);

between_questions({call, From}, {leave, Ref}, Data) ->
  handle_event({call, From}, {leave, Ref}, Data).

active_question({call, From}, next, Data) ->
  case quizmaster_helpers:is_conductor(From, Data) of
    true -> {keep_state, Data, {reply, From, {error, has_active_question}}};
    false -> {keep_state, Data, {reply, From, {error, who_are_you}}}
  end;

active_question({call, From}, timesup, Data) ->
  case quizmaster_helpers:is_conductor(From, Data) of
    true ->
      case length(maps:get(questions,Data)) == maps:get(active_question, Data) of
        true -> {stop_and_reply, normal , {reply, From, {ok, distribution, last_question, total, true}}};
        false -> NewData = maps:update(active_question, map_get(active_question, Data) + 1, Data),
          {next_state, between_questions, NewData, {reply, From, {ok, distribution, last_question, total, false}}}
      end;
    false -> {keep_state, Data, {reply, From, {error, nice_try}}}
  end;

active_question({call, From}, {guess, Ref, Index}, Data) ->
  case quizmaster_helpers:check_index_in_range(Index, Data) of
    true -> NewData = quizmaster_helpers:check_guess(Ref, Index, Data),
      {keep_state, Data, {reply, From, {ok, NewData}}};
    false -> {keep_state, Data} % ignore guess if Index out of range
  end;

active_question({call, From}, {join, Name}, Data) ->
  handle_event({call, From}, {join, Name}, Data).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

init([]) ->
  %% Set the initial state + data
  State = editable, Data = #{conductor => none, questions => [], players => #{}, active_question => 1, answered => []},
  {ok, State, Data}.

callback_mode() -> state_functions.