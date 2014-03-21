-module(swirl_ui_router).

%% public
-export([
    route/4
]).

%% public
route(<<"GET">>, [], Req, State) ->
    route(<<"GET">>, [<<"flows">>], Req, State);
route(<<"GET">>, [<<"flows">>], Req, State) ->
    {ok, View} = swirl_ui_views:flows_view(),
    reply_html(View, Req, State);
route(<<"GET">>, [<<"flows">>, Id], Req, State) ->
    {ok, View} = swirl_ui_views:flow_view(Id),
    reply_html(View, Req, State);
route(<<"GET">>, [<<"mappers">>], Req, State) ->
    {ok, View} = swirl_ui_views:mappers_view(),
    reply_html(View, Req, State);
route(<<"GET">>, [<<"processes">>, Pid], Req, State) ->
    {ok, View} = swirl_ui_views:process_view(Pid),
    reply_html(View, Req, State);
route(<<"GET">>, [<<"reducers">>], Req, State) ->
    {ok, View} = swirl_ui_views:reducers_view(),
    reply_html(View, Req, State);
route(Method, Path, Req, State) ->
    error_logger:error_msg("404: ~p", [{Method, Path}]),
    {ok, View} = swirl_ui_views:not_found_view(),
    reply_html(404, View, Req, State).

%% private
reply_html(Body, Req, State) ->
    reply_html(200, Body, Req, State).

reply_html(Status, Body, Req, State) ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_req:reply(Status, Headers, Body, Req),
    {ok, Req2, State}.
