-module(swirl_ui).

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
start() ->
    application:ensure_all_started(?MODULE).

%% application callbacks
start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
	    {'_', [
            {'_', swirl_ui_router, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
    swirl_ui_sup:start_link().

stop(_State) ->
    ok.
