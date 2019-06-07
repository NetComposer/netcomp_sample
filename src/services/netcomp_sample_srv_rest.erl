%% @doc
-module(netcomp_sample_srv_rest).

-export([request/4]).

-include_lib("nkserver/include/nkserver_callback.hrl").

%% ===================================================================
%% Private
%% ===================================================================



%% Will change to rest_request in future
request(<<"GET">>, [], #{srv:=SrvId, span:=BaseSpan}=Req, _State) ->
    nkserver_ot:new(my_span_id, SrvId, <<"Process">>, BaseSpan),
    nkserver_ot:log(my_span_id, "my processing"),
    nkserver_ot:tags(my_span_id, #{
        <<"dkv.id">> => <<"my_dkv">>,
        <<"error">> => true
    }),
    timer:sleep(5),
    nkserver_ot:log(my_span_id, "process completed"),
    timer:sleep(2),
    nkserver_ot:log(my_span_id, "process completed2"),
    nkserver_ot:finish(my_span_id),
    {http, 200, [], <<"NkSERVER REST: OK3">>, Req};


request(Method, Path, #{srv:=SrvId,peer:=Peer}=Req, _State) ->
    lager:notice("path not found (~p, ~s): ~p from ~s", [SrvId, Method, Path, Peer]),
    {http, 404, [], <<"NkSERVER REST: Path Not Found">>, Req}.
