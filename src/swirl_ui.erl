-module(swirl_ui).

-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

-define(DEFAULT_IP, {127,0,0,1}).
-define(DEFAULT_PORT, 9090).

%% public
start() ->
    application:ensure_all_started(?MODULE).

%% application callbacks
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/assets/css/[...]", cowboy_static, {priv_dir, swirl_ui, "assets/css"}},
            {"/assets/js/[...]", cowboy_static, {priv_dir, swirl_ui, "assets/js"}},
            {'_', swirl_ui_handler, []}
        ]}
    ]),

    RanchOpts = ranch_options(),
    {ok, _} = cowboy:start_http(?MODULE, 8, RanchOpts, [
        {env, [{dispatch, Dispatch}]}
    ]),
    swirl_ui_sup:start_link().

stop(_State) ->
    ok.

%% private
ranch_options() ->
    Ip = application:get_env(?MODULE, ip, ?DEFAULT_IP),
    Port = application:get_env(?MODULE, port, ?DEFAULT_PORT),
    [{ip, Ip}, {port, Port}].
