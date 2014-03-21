-module(swirl_ui_handler).
-include_lib("../../include/swirl.hrl").

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
    flows_view(Req, State);
route(<<"GET">>, [<<"flows">>, Id], Req, State) ->
    flow_view(Id, Req, State);
route(<<"GET">>, [<<"mappers">>], Req, State) ->
    mappers_view(Req, State);
route(<<"GET">>, [<<"processes">>, Pid], Req, State) ->
    process_view(Pid, Req, State);
route(<<"GET">>, [<<"reducers">>], Req, State) ->
    reducers_view(Req, State);
route(Method, Path, Req, State) ->
    io:format("404: ~p~n", [{Method, Path}]),
    {ok, Content} = not_found_dtl:render([]),
    reply_html(404, layout(Content), Req, State).

%% views
flows_view(Req, State) ->
    Flows = swirl_config:flows(),
    FlowsFormated = swirl_ui_helpers:format_flows(Flows),
    FlowsCount = length(Flows),
    {ok, Content} = flows_dtl:render([
        {flows, FlowsFormated},
        {flows_count, FlowsCount}
    ]),
    reply_html(layout(flows, Content), Req, State).

flow_view(Id, Req, State) ->
    Id2 = swirl_ui_utils:safe_string_to_uuid(Id, <<>>),
    {ok, Content} = case swirl_flow:lookup(Id2) of
        undefined ->
            flow_dtl:render([
                {id, Id},
                {not_found, true}
            ]);
        Flow ->
            Attributes = swirl_ui_helpers:format_flow(Flow),
            flow_dtl:render([
                {id, Id},
                {attributes, Attributes}
            ])
    end,
    reply_html(layout(flows, Content), Req, State).

mappers_view(Req, State) ->
    Mappers = swirl_config:mappers(),
    MappersFormated = swirl_ui_helpers:format_mappers(Mappers),
    MappersCount = length(Mappers),
    {ok, Content} = mappers_dtl:render([
        {mappers, MappersFormated},
        {mappers_count, MappersCount}
    ]),
    reply_html(layout(mappers, Content), Req, State).

process_view(Pid, Req, State) ->
    Pid2 = swirl_ui_utils:safe_binary_to_pid(Pid, <<>>),
    {ok, Content} = case process_info(Pid2) of
        undefine ->
            process_dtl:render([
                {pid, Pid},
                {not_found, true}
            ]);
        ProcessInfo ->
            Attributes = swirl_ui_helpers:format_process_info(ProcessInfo),
            process_dtl:render([
                {pid, Pid},
                {attributes, Attributes}
            ])
    end,
    reply_html(layout(undefined, Content), Req, State).

reducers_view(Req, State) ->
    Reducers = swirl_config:reducers(),
    ReducersCount = length(Reducers),
    ReducersFormated = swirl_ui_helpers:format_reducers(Reducers),
    {ok, Content} = reducers_dtl:render([
        {reducers, ReducersFormated},
        {reducers_count, ReducersCount}
    ]),
    reply_html(layout(reducers, Content), Req, State).

%% private
split_path(<<"/">>) ->
    [];
split_path(<<"/", Rest/binary>>) ->
    binary:split(Rest, <<"/">>, [global, trim]).
