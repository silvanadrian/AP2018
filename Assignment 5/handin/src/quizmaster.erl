-module(quizmaster).
-behaviour(gen_statem).
%% API exports.
-export([start/0, add_question/2, get_questions/1,
  play/1, next/1, timesup/1,
  join/2, leave/2, guess/3]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([editable/3,between_questions/3,active_question/3]).

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

timesup(_Arg0) ->
  erlang:error(not_implemented).

join(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

leave(_Arg0, _Arg1) ->
  erlang:error(not_implemented).

guess(_Arg0, _Arg1, _Arg2) ->
  erlang:error(not_implemented).

handle_event({call, From}, get_questions, Data) ->
  Questions = maps:get(questions, Data),
  {keep_state, Data , {reply, From, Questions}};
handle_event({call, From}, play, Data) ->
  case Data of
    [] -> {keep_state, Data, {reply, From, {error, no_questions}}};
    _ -> {Pid, _} = From,
      Conductor = maps:update(conductor, Pid, Data),
      {next_state, between_questions, Conductor, {reply, From, ok}}
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
editable(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

between_questions({call, From}, next, Data) ->
  {Pid, _} = From,
  case Pid == maps:get(conductor, Data) of
    true -> Question = lists:nth(maps:get(active_question, Data), maps:get(questions, Data)),
            NewData = maps:update(active_question, map_get(active_question, Data) + 1, Data),
            {next_state, active_question, NewData, {reply, From, {ok, Question}}};
    false -> {keep_state, Data, {reply, From, {error, who_are_you}}}
  end.

active_question({call, From}, next, Data) ->
  {Pid, _} = From,
  case Pid == maps:get(conductor, Data) of
    true -> {keep_state, Data, {reply, From, {error, has_active_question}}};
    false -> {keep_state, Data, {reply, From, {error, who_are_you}}}
  end.

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

init([]) ->
  %% Set the initial state + data.  Data is a empty List
  State = editable, Data = #{conductor => none, questions => [], players => #{}, active_question => 1},
  {ok, State, Data}.

callback_mode() -> state_functions.



