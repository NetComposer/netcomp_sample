%% @doc
-module(netcomp_sample_srv_pgsql).

-export([pgsql_init/1]).
-export([insert/2, get/1, drop/0]).

-include_lib("nkserver/include/nkserver_callback.hrl").


%% ===================================================================
%% Service callbacks
%% ===================================================================


%% @doc
pgsql_init(?MODULE) ->
    lager:error("NKLOG PGSQL INIT"),
    init(10).



%% ===================================================================
%% Utilities
%% ===================================================================


%% @doc
insert(Key, Value) ->
    Q = [
        <<"INSERT INTO kv VALUES (">>,
        nkpgsql_util:quote(Key), $,,
        nkpgsql_util:quote(Value), $)
    ],
    case nkpgsql:query(?MODULE, Q) of
        {ok, _, _Meta} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc
get(Key) ->
    Q = [
        <<"SELECT value FROM kv WHERE key=">>,
        nkpgsql_util:quote(Key)
    ],
    case nkpgsql:query(?MODULE, Q) of
        {ok, [[]], _Meta} ->
            {error, key_not_found};
        {ok, [[{Value}]], _Meta} ->
            {ok, Value};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
drop() ->
    Q = <<"
        DROP TABLE IF EXISTS versions CASCADE;
        DROP TABLE IF EXISTS kv CASCADE;
    ">>,
    case nkpgsql:query(?MODULE, Q) of
        {ok, _, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Internal
%% ===================================================================

%% @private
init(Tries) when Tries > 0 ->
    case nkpgsql:query(?MODULE, <<"SELECT id,version FROM versions">>) of
        {ok, [Rows], _} ->
            case maps:from_list(Rows) of
                #{
                    <<"kv">> := KvVsn
                } ->
                    case KvVsn  of
                        <<"1">> ->
                            lager:notice("detected database at last version"),
                            ok;
                        _ ->
                            lager:warning("detected database at wrong version"),
                            ok
                    end;
                _ ->
                    lager:error("unrecognized database!"),
                    {error, database_unrecognized}
            end;
        {error, relation_unknown} ->
            lager:warning("database not found: Creating it."),
            case nkpgsql:query(?MODULE, create_database_query()) of
                {ok, _, _} ->
                    ok;
                {error, Error} ->
                    lager:error("Could not create database: ~p", [Error]),
                    {error, Error}
            end;
        {error, Error} ->
            lager:notice("could not create database: ~p (~p tries left)", [Error, Tries]),
            timer:sleep(1000),
            init(Tries-1)
    end;

init(_Tries) ->
    {error, database_not_available}.



%% @private
create_database_query() ->
    <<"
        -- Comment
        BEGIN;
        CREATE TABLE versions (
            id TEXT PRIMARY KEY NOT NULL,
            version TEXT NOT NULL
        );
        CREATE TABLE kv (
            key TEXT PRIMARY KEY NOT NULL,
            value TEXT NOT NULL
        );
        CREATE INDEX key_idx on kv (key);
        INSERT INTO versions VALUES ('kv', '1');
        COMMIT;
    ">>.


