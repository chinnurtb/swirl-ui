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
    {Method, Req2} = cowboy_req:method(Req),
    {Path, Req3} = cowboy_req:path(Req2),
    Path2 = cowboy_http:urldecode(Path),
    SplitPath = swirl_ui_utils:split_path(Path2),
    swirl_ui_router:route(Method, SplitPath, Req3, State).

terminate(_Reason, _Req, _State) ->
    ok.