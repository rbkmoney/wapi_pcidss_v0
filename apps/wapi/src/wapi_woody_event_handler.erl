-module(wapi_woody_event_handler).

-behaviour(woody_event_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("identdocstore_proto/include/identdocstore_identity_document_storage_thrift.hrl").
-include_lib("woody/src/woody_defs.hrl").

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(Event, RpcID, RawMeta, Opts) ->
    FilteredMeta = filter_meta(RawMeta),
    scoper_woody_event_handler:handle_event(Event, RpcID, FilteredMeta, Opts).

filter_meta(RawMeta) ->
    case RawMeta of
        #{args := Args} ->
            RawMeta#{args => filter_args(Args)};
        #{result := Result} ->
            RawMeta#{result => filter_result(Result)};
        _ ->
            RawMeta
    end.

filter_result({ok, Result}) -> {ok, filter(Result)};
filter_result({system, SystemError}) -> {system, filter(SystemError)};
filter_result({exception, Exception}) -> {exception, filter(Exception)};
filter_result(Result) -> filter(Result).

filter_args(Args) -> filter(Args).

filter(L) when is_list(L) -> [filter(E) || E <- L];
filter(M) when is_map(M) -> maps:map(fun (_K, V) -> filter(V) end, M);

%% damsel storage
filter(#'CardData'{cardholder_name = CN, cvv = CV}) ->
    { %% Make dialyzer happy
        'CardData',
        <<"****************">>,
        {'ExpDate', <<"****">>},
        CN,
        CV
    };

%% Leave all other untouched
filter(V) -> V.
