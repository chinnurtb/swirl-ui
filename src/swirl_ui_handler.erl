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
    Headers = [{<<"content-type">>, <<"text/html">>}],
    {ok, Body} = index(),
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

index() ->
    % mappers
    Mappers = swirl_config:mappers(),
    MappersFormated = swirl_ui_helpers:format_mappers(Mappers),
    MappersCount = length(Mappers),

    % reducers
    Reducers = swirl_config:reducers(),
    ReducersCount = length(Reducers),
    ReducersFormated = swirl_ui_helpers:format_reducers(Reducers),

    index_dtl:render([
        {mappers_count, MappersCount},
        {mappers, MappersFormated},
        {reducers_count, ReducersCount},
        {reducers, ReducersFormated}
    ]).
