%% @doc
-module(netcomp_sample_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(application).

-export([start/0, start/1, start/2, stop/1]).
-export([get/1, get/2, put/2, del/1]).

-include("netcomp_sample.hrl").

-define(APP, netcomp_sample).
-compile({no_auto_import, [get/1, put/2]}).

%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts NkACTOR stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    start(permanent).


%% @doc Starts NkACTOR stand alone.
-spec start(permanent|transient|temporary) -> 
    ok | {error, Reason::term()}.

start(Type) ->
    case application:ensure_all_started(?APP, Type) of
        {ok, _Started} ->
            ok;
        Error ->
            Error
    end.


%% @doc
start(_Type, _Args) ->
    Syntax = #{
        start_services => boolean,
        listen => #{
            url => binary,
            opts => nkpacket_syntax:safe_syntax(),
            debug => {list, {atom, [ws, http, nkpacket]}},
            '__mandatory' => url
        },
        db => #{
            targets => {list, #{
                url => binary,
                weight => {integer, 1, 1000},
                pool => {integer, 1, 1000},
                '__mandatory' => url
            }},
            flavour => {atom, [postgresql, cockroachdb, yugabyte]},
            database => binary,
            debug => boolean
        },
        '__mandatory' => [start_services, listen, db]
    },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = netcomp_sample_sup:start_link(),
            {ok, Vsn} = application:get_key(netcomp_sample, vsn),
            lager:info("NETCOMP SAMPLE v~s has started.", [Vsn]),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.


%% @private OTP standard stop callback
stop(_) ->
    ok.


%% @doc gets a configuration value
get(Key) ->
    get(Key, undefined).


%% @doc gets a configuration value
get(Key, Default) ->
    nklib_config:get(?APP, Key, Default).


%% @doc updates a configuration value
put(Key, Value) ->
    nklib_config:put(?APP, Key, Value).


%% @doc updates a configuration value
del(Key) ->
    nklib_config:del(?APP, Key).
