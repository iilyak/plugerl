# What it is

- `Plugerl` is a simple ets based registry for extension points in your app.
- It dispatches the calls to multiple plugin handlers

# Plugin

Plugin is a definition of extension points in your application.
Here is an example of a simple plugin:

```erlang
-module(myapp_plug).

-export([behaviour_info/1, register/1, start/2, foo/3, bar/4, baz/2]).

behaviour_info(callbacks) ->
    plugerl:define_callbacks([
         %% notice that arity is incremented to accommodate State
         {foo, 4},
         {bar, 5},
         {baz, 3}
    ]).

register(Engine) ->
    plugerl:register_plugin(Engine, ?MODULE).

start(Engine, Args) ->
    plugerl:start_plugin(Engine, Args)

foo(A, B, C) ->
    plugerl:call(?MODULE, foo, [A, B, C]).

bar(A, B, C, D) ->
    plugerl:notify(?MODULE, bar, [A, B, C, D]).

baz(A, B) ->
    plugerl:handle(?MODULE, baz, [A, B]).

```

# Plugin handler implementation

Pluger handler is a module which implements the actual behaviour you want.
Below is an example of simple handler

```erlang
-module(myapp_plug_vendor).
-behaviour(myapp_plug).

-export([
    {foo, 4},
    {bar, 5},
    {init, 1},
    {terminate, 3},
    {handle_error, 2},
    {handle_info, 2}]).

register(Args, Options) -> %% called from app module
    plugerl:register_handler(myapp_plug, ?MODULE, Args, Options).

init(Args) ->
    State = Args,
    {ok, State}

foo(A, B, C, State) ->
    ok.

bar(A, B, C, D, State) ->
    ok.

terminate(Reason, State) ->
    ok.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_error(Function, Reason, Args, State) ->
    ok.
```

# Supported operations

- call - call every handler and wait while all of them finish
- notify - send a message to every handler and keep going
- iterate - call passed function for every handler. There is also support for quick exit
- handle - call every handler until we get `{{reply, Result}, State}` or `{{error, Reason}, State}`
- fanout - more efficient `call` without an ability to access `state`

# Architecture

![plugerl](https://cloud.githubusercontent.com/assets/9804420/6623984/ac9b8472-c8a3-11e4-9632-235cb495839b.jpg)

[source](https://github.com/iilyak/plugerl/wiki/architecture.uml)
