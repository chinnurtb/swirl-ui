-module(swirl_ui_views).

-export([
    flow_view/1,
    flows_view/0,
    mappers_view/0,
    not_found_view/0,
    process_view/1,
    reducers_view/0
]).

-define(BASE_URL, <<"http://localhost:9090/">>).

flow_view(Id) ->
    Id2 = swirl_ui_utils:safe_string_to_uuid(Id, <<>>),
    Options = case swirl_flow:lookup(Id2) of
        undefined -> [{not_found, true}];
        Flow ->
            Attributes = swirl_ui_helpers:format_flow(Flow),
            [{attributes, Attributes}]
    end,
    {ok, Content} = flow_dtl:render([{id, Id} | Options]),
    layout(flows, Content).

flows_view() ->
    Flows = swirl_config:flows(),
    FlowsFormated = swirl_ui_helpers:format_flows(Flows),
    FlowsCount = length(Flows),
    {ok, Content} = flows_dtl:render([
        {flows, FlowsFormated},
        {flows_count, FlowsCount}
    ]),
    layout(flows, Content).

mappers_view() ->
    Mappers = swirl_config:mappers(),
    MappersFormated = swirl_ui_helpers:format_mappers(Mappers),
    MappersCount = length(Mappers),
    {ok, Content} = mappers_dtl:render([
        {mappers, MappersFormated},
        {mappers_count, MappersCount}
    ]),
    layout(mappers, Content).

not_found_view() ->
    {ok, Content} = not_found_dtl:render([]),
    layout(Content).

process_view(Pid) ->
    Pid2 = swirl_ui_utils:safe_binary_to_pid(Pid, <<>>),
    Options = case process_info(Pid2) of
        undefined -> [{not_found, true}];
        ProcessInfo ->
            Attributes = swirl_ui_helpers:format_process_info(ProcessInfo),
            [{attributes, Attributes}]
    end,
    {ok, Content} = process_dtl:render([{pid, Pid} | Options]),
    layout(undefined, Content).

reducers_view() ->
    Reducers = swirl_config:reducers(),
    ReducersCount = length(Reducers),
    ReducersFormated = swirl_ui_helpers:format_reducers(Reducers),
    {ok, Content} = reducers_dtl:render([
        {reducers, ReducersFormated},
        {reducers_count, ReducersCount}
    ]),
    layout(reducers, Content).

%% private
layout(Content) ->
    layout(undefined, Content).

layout(NavActive, Content) ->
    layout_dtl:render([
        {NavActive, true},
        {base_url, ?BASE_URL},
        {content, Content},
        {node, node()}
    ]).
