-module(swirl_ui_helpers).
% TODO: add getters for #flow {} and make it opaque
-include_lib("../../include/swirl.hrl").

-export([
    format_flows/1,
    format_mappers/1,
    format_reducers/1
]).

%% public
format_flows([]) ->
    [];
format_flows([{{flow, FlowId}, #flow {
        module        = Module,
        stream_name   = StreamName,
        stream_filter = StreamFilter,
        % heartbeat     = Heartbeat,
        % mapper_flush  = MapperFlush,
        mapper_nodes  = MapperNodes,
        % reducer_flush = ReducerFlush,
        reducer_node  = ReducerNode% ,
        % timestamp     = Timestamp
    }} | T]) ->

    Row = [uuid:uuid_to_string(FlowId), Module, StreamName, StreamFilter,
        length(MapperNodes), ReducerNode],
    [Row | format_flows(T)].

format_mappers([]) ->
    [];
format_mappers([{{mapper, FlowId}, Pid} | T]) ->
    [[pid_to_list(Pid), uuid:uuid_to_string(FlowId)] | format_mappers(T)].

format_reducers([]) ->
    [];
format_reducers([{{reducer, FlowId}, Pid} | T]) ->
    [[pid_to_list(Pid), uuid:uuid_to_string(FlowId)] | format_reducers(T)].


