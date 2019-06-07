%% @doc
-module(netcomp_sample_test).
-export([start/0, stop/0, status/0]).
-export([request/4]).


%%%% Util

start() ->
    ListenConfig1 = #{
        url => "http://all:9001",
        debug => [http, nkpacket],
        opentrace_filter => opentrace_filter()
    },
    ListenConfig2 = ListenConfig1#{
        % Tell nkserver not to use module 'my_rest' to find callbacks,
        % but find them in this same module
        use_module => ?MODULE
    },
    %nkserver:start_link(nkrest, my_rest, ListenConfig2).
    nkserver:start_link(<<"Rest">>, my_rest, ListenConfig2).


stop() ->
    nkserver:stop(my_rest).


status() ->
    nkserver:get_all_status().


opentrace_filter() ->
    "
        my_spans() -> send.
        count() -> count('request', span_name, final_result).
    ".


%%%% Service callbacks

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






