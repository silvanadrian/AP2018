-module(quizmaster).
-behaviour(gen_statem).
%% API exports.
-export([start/0, add_question/2, get_questions/1,
  play/1, next/1, timesup/1,
  join/2, leave/2, guess/3]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([editable/3]).

start() ->
  gen_statem:start({local, quizmaster_server}, ?MODULE, [], []).

add_question(Server, {Description, Answers}) ->
  gen_statem:call(Server, {add_question, {Description, Answers}}).

get_questions(Server) ->
  gen_statem:call(Server, get_questions).

play(Server) ->
  gen_statem:call(Server, play).

next(_Arg0) ->
  erlang:error(not_implemented).

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
    _ -> Conductor = maps:update(conductor, From, Data),
      {next_state, playing, Conductor, {reply, From, ok}}
  end.


editable({call, From}, {add_question, Question}, Data) ->
  OldQuestions = maps:get(questions, Data),
  UpdatedQuestions = maps:update(questions, lists:append(OldQuestions, [Question]), Data),
  {keep_state, UpdatedQuestions , {reply, From, ok}};
editable(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

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



