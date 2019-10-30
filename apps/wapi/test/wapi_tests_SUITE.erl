-module(wapi_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_config_thrift.hrl").
-include_lib("binbase_proto/include/binbase_binbase_thrift.hrl").
-include_lib("wapi_dummy_data.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([init/1]).

-export([
    store_bank_card_ok_test/1
]).

-define(WAPI_IP                     , "::").
-define(WAPI_PORT                   , 8080).
-define(WAPI_HOST_NAME              , "localhost").
-define(WAPI_URL                    , ?WAPI_HOST_NAME ++ ":" ++ integer_to_list(?WAPI_PORT)).

-define(badresp(Code), {error, {invalid_response_code, Code}}).

-type test_case_name()  :: atom().
-type config()          :: [{atom(), any()}].
-type group_name()      :: atom().

-behaviour(supervisor).

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {#{strategy => one_for_all, intensity => 1, period => 1}, []}}.

-spec all() ->
    [test_case_name()].
all() ->
    [
        {group, all}
    ].

-spec groups() ->
    [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {all, [],
            [
                store_bank_card_ok_test
            ]
        }
    ].

%%
%% starting/stopping
%%
-spec init_per_suite(config()) ->
    config().
init_per_suite(Config) ->
    SupPid = start_mocked_service_sup(),
    Apps =
        wapi_ct_helper:start_app(woody)  ++
        wapi_ct_helper:start_app(scoper) ++
        start_wapi(Config),
    [{apps, lists:reverse(Apps)}, {suite_test_sup, SupPid} | Config].

-spec end_per_suite(config()) ->
    _.
end_per_suite(C) ->
    _ = stop_mocked_service_sup(?config(suite_test_sup, C)),
    [application:stop(App) || App <- proplists:get_value(apps, C)],
    ok.

-spec init_per_group(group_name(), config()) ->
    config().
init_per_group(_, Config) ->
    BasePermissions = {destinations,<<"payment_resources">>},
    Token = wapi_ct_helper:issue_token(BasePermissions, unlimited),
    Context = get_context(Token),
    [{context, Context} | Config].

-spec end_per_group(group_name(), config()) ->
    _.
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(test_case_name(), config()) ->
    config().
init_per_testcase(_Name, C) ->
    [{test_sup, start_mocked_service_sup()} | C].

-spec end_per_testcase(test_case_name(), config()) ->
    config().
end_per_testcase(_Name, C) ->
    stop_mocked_service_sup(?config(test_sup, C)),
    ok.

%%% Tests

-spec store_bank_card_ok_test(_) ->
    _.
store_bank_card_ok_test(Config) ->
    Result = mock_services([
        {cds_storage, fun
            ('PutCardData', [
                #'cds_CardData'{pan = <<"411111", _:6/binary, Mask:4/binary>>},
                _SessionData
            ]) ->
                {ok, #'cds_PutCardDataResult'{
                    bank_card = #cds_BankCard{
                        token = ?STRING,
                        bin = <<"411111">>,
                        last_digits = Mask
                    },
                    session_id = <<"1234563346321">>
                }}
        end},
        {binbase, fun('Lookup', _) ->
            {ok, ?BINBASE_LOOKUP_RESULT(<<"VISA">>)} end}
    ], Config),
    application:set_env(wapi, service_urls, Result),
    {ok, #{
            <<"authData">> := <<"1234563346321">>,
            <<"bin">> := <<"411111">>,
            <<"lastDigits">> := <<"1111">>,
            <<"paymentSystem">> := <<"visa">>
    }} =
        wapi_test_client:store_bank_card(?config(context, Config), #{
        <<"cardNumber">> => <<"4111111111111111">>,
        <<"cardHolder">> => <<"ALEXANDER WEINERSCHNITZEL">>,
        <<"expDate">> => <<"08/27">>,
        <<"cvv">> => <<"232">>
    }).

%%

start_wapi(Config) ->
    WapiEnv = [
        {ip, ?WAPI_IP},
        {port, ?WAPI_PORT},
        {service_type, real},
        {realm, <<"TEST">>},
        {authorizers, #{
            jwt => #{
                signee => 'wallet-api',
                keyset => #{
                    'wallet-api' => {pem_file, get_keysource("keys/local/private.pem", Config)}
                }
            }
        }}
    ],
    wapi_ct_helper:start_app(wapi, WapiEnv).

start_mocked_service_sup() ->
    {ok, SupPid} = supervisor:start_link(?MODULE, []),
    _ = unlink(SupPid),
    SupPid.

stop_mocked_service_sup(SupPid) ->
    exit(SupPid, shutdown).

mock_services(Services, SupOrConfig) ->
    mock_services_(Services, SupOrConfig).

mock_services_(Services, Config) when is_list(Config) ->
    mock_services_(Services, ?config(test_sup, Config));

mock_services_(Services, SupPid) when is_pid(SupPid) ->
    Name = lists:map(fun get_service_name/1, Services),
    Port = get_random_port(),
    {ok, IP} = inet:parse_address(?WAPI_IP),
    ChildSpec = woody_server:child_spec(
        {dummy, Name},
        #{
            ip => IP,
            port => Port,
            event_handler => scoper_woody_event_handler,
            handlers => lists:map(fun mock_service_handler/1, Services)
        }
    ),
    {ok, _} = supervisor:start_child(SupPid, ChildSpec),
    lists:foldl(
        fun (Service, Acc) ->
            ServiceName = get_service_name(Service),
            Acc#{ServiceName => make_url(ServiceName, Port)}
        end,
        #{},
        Services
    ).

get_service_name({ServiceName, _Fun}) ->
    ServiceName;
get_service_name({ServiceName, _WoodyService, _Fun}) ->
    ServiceName.

mock_service_handler({ServiceName, Fun}) ->
    mock_service_handler(ServiceName, wapi_woody_client:get_service_modname(ServiceName), Fun);
mock_service_handler({ServiceName, WoodyService, Fun}) ->
    mock_service_handler(ServiceName, WoodyService, Fun).

mock_service_handler(ServiceName, WoodyService, Fun) ->
    {make_path(ServiceName), {WoodyService, {wapi_dummy_service, #{function => Fun}}}}.

make_url(ServiceName, Port) ->
    iolist_to_binary(["http://", ?WAPI_HOST_NAME, ":", integer_to_list(Port), make_path(ServiceName)]).

make_path(ServiceName) ->
    "/" ++ atom_to_list(ServiceName).

% TODO not so failproof, ideally we need to bind socket first and then give to a ranch listener
get_random_port() ->
    rand:uniform(32768) + 32767.

get_context(Token) ->
    get_context(?WAPI_URL, Token, 10000, ipv4).

get_context(Url, Token, Timeout, Protocol) ->
    get_context(Url, Token, Timeout, Protocol, default_event_handler()).

get_context(Url, Token, Timeout, Protocol, EventHandler) ->
    #{
        url           => Url,
        token         => Token,
        timeout       => Timeout,
        protocol      => Protocol,
        event_handler => EventHandler
    }.

default_event_handler() ->
    fun(_Type, _Code, _Duration) ->
        ok
    end.

get_keysource(Key, Config) ->
    filename:join(?config(data_dir, Config), Key).
