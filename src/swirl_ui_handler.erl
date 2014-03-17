-module(swirl_ui_handler).

-behavior(cowboy_http_handler).
-export([
    init/3,
    handle/2,
    terminate/3
]).

-define(BASE_URL, <<"http://localhost:9090/">>).

%% cowboy_http_handler callbacks
init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    Path2 = cowboy_http:urldecode(Path),
    SplitPath = split_path(Path2),
    route(Method, SplitPath, Req3, State).

terminate(_Reason, _Req, _State) ->
    ok.

%% private
layout(Content) ->
    layout(undefined, Content).

layout(NavActive, Content) ->
    {ok, Layout} = layout_dtl:render([
        {NavActive, true},
        {base_url, ?BASE_URL},
        {content, Content},
        {node, node()}
    ]),
    Layout.

reply_html(Body, Req, State) ->
    reply_html(200, Body, Req, State).

reply_html(Status, Body, Req, State) ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
    {ok, Req2, State}.

route(<<"GET">>, [], Req, State) ->
    route(<<"GET">>, [<<"flows">>], Req, State);
route(<<"GET">>, [<<"flows">>], Req, State) ->
    Flows = swirl_config:flows(),
    FlowsFormated = swirl_ui_helpers:format_flows(Flows),
    FlowsCount = length(Flows),
    {ok, Content} = flows_dtl:render([
        {flows, FlowsFormated},
        {flows_count, FlowsCount}
    ]),
    reply_html(layout(flows, Content), Req, State);
route(<<"GET">>, [<<"flows">>, Id], Req, State) ->
    {ok, Content} = flow_dtl:render([{id, Id}]),
    reply_html(layout(flows, Content), Req, State);
route(<<"GET">>, [<<"mappers">>], Req, State) ->
    Mappers = swirl_config:mappers(),
    MappersFormated = swirl_ui_helpers:format_mappers(Mappers),
    MappersCount = length(Mappers),
    {ok, Content} = mappers_dtl:render([
        {mappers, MappersFormated},
        {mappers_count, MappersCount}
    ]),
    reply_html(layout(mappers, Content), Req, State);
route(<<"GET">>, [<<"mappers">>, Pid], Req, State) ->
    {ok, Content} = mapper_dtl:render([{pid, Pid}]),
    reply_html(layout(mappers, Content), Req, State);
route(<<"GET">>, [<<"reducers">>], Req, State) ->
    Reducers = swirl_config:reducers(),
    ReducersCount = length(Reducers),
    ReducersFormated = swirl_ui_helpers:format_reducers(Reducers),
    {ok, Content} = reducers_dtl:render([
        {reducers, ReducersFormated},
        {reducers_count, ReducersCount}
    ]),
    reply_html(layout(reducers, Content), Req, State);
route(<<"GET">>, [<<"reducers">>, Pid], Req, State) ->
    {ok, Content} = reducer_dtl:render([{pid, Pid}]),
    reply_html(layout(reducers, Content), Req, State);
route(Method, Path, Req, State) ->
    io:format("404: ~p~n", [{Method, Path}]),
    {ok, Content} = not_found_dtl:render([]),
    reply_html(404, layout(Content), Req, State).

split_path(<<"/">>) ->
    [];
split_path(<<"/", Rest/binary>>) ->
    binary:split(Rest, <<"/">>, [global, trim]).
