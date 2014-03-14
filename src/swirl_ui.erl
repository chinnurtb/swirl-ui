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
            {"/", swirl_ui_handler, []},
            {"/assets/css/[...]", cowboy_static,
                {priv_dir, swirl_ui, "assets/css"}},
            {"/assets/js/[...]", cowboy_static,
                {priv_dir, swirl_ui, "assets/js"}}
        ]}
    ]),

    RanchOpts = ranch_options(),
    {ok, _} = cowboy:start_http(?MODULE, 100, RanchOpts, [
        {env, [{dispatch, Dispatch}]}
    ]),
    swirl_ui_sup:start_link().

stop(_State) ->
    ok.

%% private
ranch_options() ->
    {ok, Ip} = application:get_env(?MODULE, ip),
    {ok, Port} = application:get_env(?MODULE, port),
    [{ip, Ip}, {port, Port}].

