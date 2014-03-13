-module(swirl_ui_helpers).

-export([
    format_mappers/1,
    format_reducers/1
]).

%% public
format_mappers([]) ->
    [];
format_mappers([{{mapper, FlowId}, Pid, {FlowMod, FlowOpts, ReducerNode}} | T]) ->
    StreamFilter = swirl_utils:lookup(stream_filter, FlowOpts),
    [[uuid:uuid_to_string(FlowId), pid_to_list(Pid), FlowMod, StreamFilter, ReducerNode] | format_mappers(T)].

format_reducers([]) ->
    [];
format_reducers([{{reducer, FlowId}, Pid, {FlowMod, FlowOpts, MapperNodes}} | T]) ->
    StreamFilter = swirl_utils:lookup(stream_filter, FlowOpts),
    [[uuid:uuid_to_string(FlowId), pid_to_list(Pid), FlowMod, StreamFilter, MapperNodes] | format_reducers(T)].
