-module(swirl_ui_router).

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
    {ok, Body} = index_dtl:render([]),
	{ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
