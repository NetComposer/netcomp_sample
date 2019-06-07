%% @doc Main supervisor
-module(netcomp_sample_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-behaviour(supervisor).

-export([init/1, start_link/0]).

-include("netcomp_sample.hrl").

-define(PGSQL_SRV, netcomp_sample_srv_pgsql).
-define(REST_SRV, netcomp_sample_srv_rest).

%% @private
start_link() ->
    %PgSqlConfig = netcomp_sample_app:get(db),
    RestConfig = netcomp_sample_app:get(listen),
    ChildsSpec = case netcomp_sample_app:get(start_services) of
        true ->
            lager:notice("Starting services"),
            [
                %nkserver:get_sup_spec(nkpgsql, ?PGSQL_SRV, PgSqlConfig),
                nkserver:get_sup_spec(<<"Rest">>, ?REST_SRV, RestConfig)
            ];
        false ->
            lager:notice("Not starting services"),
            []
    end,
    supervisor:start_link({local, ?MODULE}, ?MODULE, 
                            {{one_for_one, 10, 60}, ChildsSpec}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


