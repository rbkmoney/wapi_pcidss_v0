-module(wapi_graceful_shutdown_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("fistful_proto/include/ff_proto_base_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("wapi_dummy_data.hrl").

-define(NUMBER_OF_WORKERS, 10).

-export([init/1]).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-behaviour(supervisor).

-export([
    shutdown_test/1,
    request_interrupt_test/1
]).

-type config()         :: ct_helper:config().
-type test_case_name() :: ct_helper:test_case_name().
-type group_name()     :: ct_helper:group_name().
-type test_return()    :: _ | no_return().

-spec all() -> [test_case_name() | {group, group_name()}].

all() ->
    [{group, default}].

-spec groups() -> [{group_name(), list(), [test_case_name()]}].

groups() ->
    [
        {default, [
            shutdown_test,
            request_interrupt_test
        ]}
    ].

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec init_per_suite(config()) -> config().

init_per_suite(C) ->
    wapi_ct_helper:init_suite(?MODULE, C).

-spec end_per_suite(config()) -> _.

end_per_suite(C) ->
    _ = wapi_ct_helper:stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) -> config().

init_per_group(default, Config) ->
    Token = wapi_ct_helper:issue_token([{[party], write}, {[party], read}], unlimited),
    [{context, wapi_ct_helper:get_context(Token)} | Config].

-spec end_per_group(group_name(), config()) -> _.

end_per_group(_Group, C) ->
    proplists:delete(context, C),
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, wapi_ct_helper:start_mocked_service_sup(?MODULE)} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    _ = application:start(wapi),
    wapi_ct_helper:stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec shutdown_test(config()) -> test_return().

shutdown_test(C) ->
    Context = ?config(context, C),
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services([
        {binbase, fun('Lookup', _) ->
                ok = timer:sleep(2000),
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
        end},
        {cds_storage, fun
            ('PutCard',    _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
            ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
        end}
    ], C),
    Bin        = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">>        := Bin,
        <<"lastDigits">> := LastDigits,
        <<"paymentSystem">> := <<"visa">>
    }} = wapi_client_payres:store_bank_card(Context, ?STORE_BANK_CARD_REQUEST(CardNumber)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = application:stop(wapi),
    ok = receive_loop(fun(Result) -> {ok, _} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun(Result) -> {error, econnrefused} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

-spec request_interrupt_test(config()) ->
    _.
request_interrupt_test(C) ->
    Context = ?config(context, C),
    CardNumber = <<"4150399999000900">>,
    wapi_ct_helper:mock_services([
        {binbase, fun('Lookup', _) ->
                ok = timer:sleep(2000),
                {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)}
        end},
        {cds_storage, fun
            ('PutCard',    _) -> {ok, ?PUT_CARD_RESULT(CardNumber)};
            ('PutSession', _Args) -> {ok, ?PUT_SESSION_RESULT}
        end}
    ], C),
    Bin        = ?BIN(CardNumber),
    LastDigits = ?LAST_DIGITS(CardNumber),
    {ok, #{
        <<"bin">>        := Bin,
        <<"lastDigits">> := LastDigits,
        <<"paymentSystem">> := <<"visa">>
    }} = wapi_client_payres:store_bank_card(Context, ?STORE_BANK_CARD_REQUEST(CardNumber)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = timer:sleep(1000),
    ok = application:stop(wapi),
    ok = receive_loop(fun(Result) -> {ok, _} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)),
    ok = spawn_workers(Context, self(), ?NUMBER_OF_WORKERS),
    ok = receive_loop(fun(Result) -> {error, econnrefused} = Result end, ?NUMBER_OF_WORKERS, timer:seconds(20)).

%%

receive_loop(_, N, _Timeout) when N =< 0 ->
    ok;
receive_loop(MatchFun, N, Timeout) ->
    receive
        {result, Result} ->
            MatchFun(Result)
    after Timeout ->
        error(timeout)
    end,
    receive_loop(MatchFun, N - 1, Timeout).

spawn_workers(_, _, N) when N =< 0 ->
    ok;
spawn_workers(Context, ParentPID, N) ->
    erlang:spawn_link(fun() -> worker(Context, ParentPID) end),
    spawn_workers(Context, ParentPID, N - 1).

worker(Context, ParentPID) ->
    CardNumber = <<"4150399999000900">>,
    Result = wapi_client_payres:store_bank_card(Context, ?STORE_BANK_CARD_REQUEST(CardNumber)),
    ParentPID ! {result, Result}.
