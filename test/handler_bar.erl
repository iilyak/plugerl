-module(handler_bar).

-export([init/1,
         add/1,
         add/2,
         get/1,
         fail_one/2,
         reply_one/1,
         handle_info/2,
         handle_error/4,
         terminate/2]).

init([Value]) ->
    {ok, Value}.

add(A, State) ->
    {A, State + A}.

add(Value) ->
    Value + 2.

get(Value) ->
    {Value, Value}.

fail_one(A, State) ->
    {ok, State + A}.

reply_one(State) ->
    {{reply, State}, State + 1}.

handle_info({add, Arg}, State) ->
    {noreply, State + Arg}.

handle_error(Function, Reason, Args, State) ->
    plugerl_tests:save(?MODULE, handle_error, {Function, Reason, Args, State}),
    ok.

terminate(Reason, State) ->
    plugerl_tests:save(?MODULE, terminate, {Reason, State}),
    ok.
