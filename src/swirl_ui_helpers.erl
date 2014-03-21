-module(swirl_ui_helpers).
-include_lib("../../include/swirl.hrl").

-export([
    format_flow/1,
    format_flows/1,
    format_mappers/1,
    format_process_info/1,
    format_reducers/1
]).

%% public
format_flow(#flow {
        module = Module,
        module_vsn = ModuleVsn,
        stream_name = StreamName,
        stream_filter = StreamFilter,
        heartbeat = Heartbeat,
        mapper_flush = MapperFlush,
        mapper_nodes = MapperNodes,
        mapper_opts = MapperOpts,
        reducer_flush = ReducerFlush,
        reducer_node = ReducerNode,
        reducer_opts = ReducerOpts,
        started_at = StartedAt,
        start_node = StartNode
    }) ->

    [[<<"module">>, Module],
     [<<"module_vsn">>, ModuleVsn],
     [<<"stream_name">>, StreamName],
     [<<"stream_filter">>, StreamFilter],
     [<<"heartbeat">>, Heartbeat],
     [<<"mapper_nodes">>, MapperNodes],
     [<<"mapper_flush">>, MapperFlush],
     [<<"mapper_opts">>, MapperOpts],
     [<<"reducer_node">>, ReducerNode],
     [<<"reducer_flush">>, ReducerFlush],
     [<<"reducer_opts">>, ReducerOpts],
     [<<"started_at">>, format_date(StartedAt)],
     [<<"start_node">>, StartNode]].

format_flows([]) ->
    [];
format_flows([{FlowId, #flow {
        module        = Module,
        stream_name   = StreamName,
        stream_filter = StreamFilter,
        mapper_nodes  = MapperNodes,
        started_at    = StartedAt
    }} | T]) ->

    Row = [uuid:uuid_to_string(FlowId), Module, StreamName, StreamFilter,
        length(MapperNodes), format_date(StartedAt)],
    [Row | format_flows(T)].

format_mappers([]) ->
    [];
format_mappers([{FlowId, Pid} | T]) ->
    [[uuid:uuid_to_string(FlowId), pid_to_list(Pid)] | format_mappers(T)].

format_process_info(ProcessInfo) ->
    [[<<"state">>, ?L(status, ProcessInfo)],
     [<<"message_queue_len">>, ?L(message_queue_len, ProcessInfo)],
     [<<"messages">>, ?L(messages, ProcessInfo)],
     [<<"total_heap_size">>, ?L(total_heap_size, ProcessInfo)],
     [<<"heap_size">>, ?L(heap_size, ProcessInfo)],
     [<<"stack_size">>, ?L(stack_size, ProcessInfo)],
     [<<"reductions">>, ?L(reductions, ProcessInfo)]].

format_reducers([]) ->
    [];
format_reducers([{FlowId, Pid} | T]) ->
    [[uuid:uuid_to_string(FlowId), pid_to_list(Pid)] | format_reducers(T)].

%% private
format_date(Tstamp) ->
    {{Y,M,D}, {H,MM,S}} = calendar:now_to_local_time(Tstamp),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y,M,D,H,MM,S]).
