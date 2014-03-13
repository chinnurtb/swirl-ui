-module(swirl_ui_handler).

-behavior(cowboy_http_handler).
-export([
    init/3,
    handle/2,
    terminate/3
]).

%% cowboy_http_handler callbacks
init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    % headers
    Headers = [
        {<<"content-type">>, <<"text/html">>}
    ],

    % body
    Mappers = swirl_ui_helpers:format_mappers(swirl_config:mappers()),
    Reducers = swirl_ui_helpers:format_reducers(swirl_config:reducers()),
    {ok, Body} = index_dtl:render([
        {mappers, Mappers},
        {reducers, Reducers}
    ]),

    % reply
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
