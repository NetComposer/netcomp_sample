%% @doc
-module(netcomp_sample_srv_rest).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([authorize/1]).

-include_lib("nkserver/include/nkserver_callback.hrl").

%% ===================================================================
%% Private
%% ===================================================================


authorize(Req) ->
    case nklib_util:do_config_get(netcomp_sample_token) of
        <<>> ->
            {true, Req};
        Token ->
            case Req of
                #{auth:=#{token:=Token}} ->
                    {true, Req};
                _ ->
                    false
            end
    end.