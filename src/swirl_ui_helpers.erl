-module(swirl_ui_helpers).

-export([
    format_mappers/1,
    format_reducers/1
]).

-define(DEFAULT_MAPPER_FLUSH, timer:seconds(1)).
-define(DEFAULT_REDUCER_FLUSH, timer:seconds(1)).

%% public
format_mappers([]) ->
    [];
format_mappers([{{mapper, FlowId}, Pid, {FlowMod, FlowOpts, ReducerNode}} | T]) ->
    StreamName = swirl_utils:lookup(stream_name, FlowOpts),
    StreamFilter = swirl_utils:lookup(stream_filter, FlowOpts),
    Flush = swirl_utils:lookup(mapper_flush, FlowOpts, ?DEFAULT_MAPPER_FLUSH),
    FlushSec = trunc(Flush / 1000),
    [[pid_to_list(Pid), uuid:uuid_to_string(FlowId), FlowMod, StreamName,
        StreamFilter, FlushSec, ReducerNode] | format_mappers(T)].

format_reducers([]) ->
    [];
format_reducers([{{reducer, FlowId}, Pid, {FlowMod, FlowOpts, MapperNodes}} | T]) ->
    StreamName = swirl_utils:lookup(stream_name, FlowOpts),
    StreamFilter = swirl_utils:lookup(stream_filter, FlowOpts),
    Flush = swirl_utils:lookup(reducer_flush, FlowOpts, ?DEFAULT_REDUCER_FLUSH),
    FlushSec = trunc(Flush / 1000),
    [[pid_to_list(Pid), uuid:uuid_to_string(FlowId), FlowMod, StreamName,
        StreamFilter, FlushSec, length(MapperNodes)] | format_reducers(T)].


