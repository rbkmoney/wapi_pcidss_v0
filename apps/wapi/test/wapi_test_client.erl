-module(wapi_test_client).

%% API
-export([store_bank_card/2]).

-type context() :: capi_client_lib:context().

-spec store_bank_card(context(), map()) -> {ok, #{binary() => _}} | {error, term()}.

store_bank_card(Context, Request) ->
    Params = #{body => Request},
    {Url, PreparedParams, Opts} = wapi_test_client_lib:make_request(Context, Params),
    Response = swag_client_payres_payment_resources_api:store_bank_card(Url, PreparedParams, Opts),
    wapi_test_client_lib:handle_response(Response).
