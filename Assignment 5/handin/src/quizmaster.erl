-module(quizmaster).

%% API exports.
-export([start/0, add_question/2, get_questions/1,
         play/1, next/1, timesup/1,
         join/2, leave/2, guess/3]).
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
-export([editable/3]).

start() ->
  gen_statem:start({local, quizmaster_server},?MODULE,[], []).

add_question(Server, {Description, Answers}) ->
  gen_statem:call(Server, {add_question, {Description, Answers} }).

get_questions(Server) ->
  gen_statem:call(Server, get_questions).

play(_Arg0) ->
  erlang:error(not_implemented).

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

handle_event({call, From}, get_questions, Data ) ->
  {keep_state, Data, {reply, From, Data}}.

editable({call,From}, {add_question, Question}, Data) ->
  {keep_state, [Question | Data],{reply,From,ok}};
editable(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

%% Mandatory callback functions
terminate(_Reason, _State, _Data) ->
  void.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

init([]) ->
  %% Set the initial state + data.  Data is a empty List
  State = editable, Data = [],
  {ok,State,Data}.

callback_mode() -> state_functions.



