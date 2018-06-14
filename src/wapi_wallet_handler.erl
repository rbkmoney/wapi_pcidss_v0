-module(wapi_wallet_handler).

-behaviour(swag_wallet_server_logic_handler).

%% API callbacks
-export([authorize_api_key/2]).
-export([handle_request/3]).

%% @WARNING Must be refactored in case of different classes of users using this API
-define(REALM, <<"external">>).

-define(DEFAULT_INVOICE_META, #{}).
-define(DEFAULT_INVOICE_LINE_META, #{}).
-define(DEFAULT_URL_LIFETIME, 60). % seconds

-spec authorize_api_key(swag_wallet_server:operation_id(), swag_wallet_server:api_key()) ->
    Result :: false | {true, wapi_auth:context()}.

authorize_api_key(OperationID, ApiKey) ->
    _ = wapi_utils:logtag_process(operation_id, OperationID),
    wapi_auth:authorize_api_key(OperationID, ApiKey).

-type request_data() :: #{atom() | binary() => term()}.
-type processing_context() :: #{
    swagger_context := swag_wallet_server:request_context(),
    woody_context   := woody_context:ctx()
}.

-spec handle_request(
    OperationID :: swag_wallet_server:operation_id(),
    Req :: request_data(),
    SwagContext :: swag_wallet_server:request_context()
) ->
    {ok | error, swag_wallet_server_logic_handler:response()}.

handle_request(OperationID, Req, SwagContext = #{auth_context := AuthContext}) ->
    _ = lager:info("Processing request ~p", [OperationID]),
    try
        case wapi_auth:authorize_operation(OperationID, Req, AuthContext) of
            ok ->
                WoodyContext = create_woody_context(Req, AuthContext),
                Context = create_processing_context(SwagContext, WoodyContext),
                process_request(OperationID, Req, Context);
            {error, _} = Error ->
                _ = lager:info("Operation ~p authorization failed due to ~p", [OperationID, Error]),
                {error, {401, [], general_error(<<"Unauthorized operation">>)}}
        end
    catch
        error:{woody_error, {Source, Class, Details}} ->
            process_woody_error(Source, Class, Details)
    end.

-spec process_request(
    OperationID :: swag_wallet_server:operation_id(),
    Req :: request_data(),
    Context :: processing_context()
) ->
    {Code :: non_neg_integer(), Headers :: [], Response :: #{}}.

process_request(_Function, _Req, _Context) ->
    {200, [], #{}}.

%% process_request('CreateInvoice', Req, Context) ->
%%     PartyID = get_party_id(Context),
%%     try
%%         Call = {invoicing, 'Create', [encode_invoice_params(PartyID, maps:get('InvoiceParams', Req))]},
%%         service_call_with([user_info, party_creation], Call, Context)
%%     of
%%         {ok, #'payproc_Invoice'{invoice = Invoice}} ->
%%             {ok, {201, [], make_invoice_and_token(Invoice, PartyID)}};
%%         {exception, Exception} ->
%%             case Exception of
%%                 #'InvalidRequest'{errors = Errors} ->
%%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
%%                 #payproc_ShopNotFound{} ->
%%                     {ok, {400, [], logic_error(invalidShopID, <<"Shop not found">>)}};
%%                 #payproc_InvalidPartyStatus{} ->
%%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%%                 #payproc_InvalidShopStatus{} ->
%%                     {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}}
%%             end
%%     catch
%%         invoice_cart_empty ->
%%             {ok, {400, [], logic_error(invalidInvoiceCart, <<"Wrong size. Path to item: cart">>)}};
%%         invalid_invoice_cost ->
%%             {ok, {400, [], logic_error(invalidInvoiceCost, <<"Invalid invoice amount">>)}}
%%     end.

%% process_request('CreatePayment', Req, Context) ->
%%     InvoiceID = maps:get('invoiceID', Req),
%%     PaymentParams = maps:get('PaymentParams', Req),
%%     Flow = genlib_map:get(<<"flow">>, PaymentParams, #{<<"type">> => <<"PaymentFlowInstant">>}),
%%     Result =
%%         try
%%             Params =  #payproc_InvoicePaymentParams{
%%                 'payer' = encode_payer_params(genlib_map:get(<<"payer">>, PaymentParams)),
%%                 'flow' = encode_flow(Flow)
%%             },
%%             Call = {invoicing, 'StartPayment', [InvoiceID, Params]},
%%             service_call_with([user_info], Call, Context)
%%         catch
%%             throw:Error when Error =:= invalid_token orelse Error =:= invalid_payment_session ->
%%                 {error, Error}
%%         end,

%%     case Result of
%%         {ok, Payment} ->
%%             {ok, {201, [], decode_invoice_payment(InvoiceID, Payment, Context)}};
%%         {exception, Exception} ->
%%             case Exception of
%%                 #payproc_InvalidInvoiceStatus{} ->
%%                     {ok, {400, [], logic_error(invalidInvoiceStatus, <<"Invalid invoice status">>)}};
%%                 #payproc_InvoicePaymentPending{} ->
%%                     {ok, {400, [], logic_error(invoicePaymentPending, <<"Invoice payment pending">>)}};
%%                 #'InvalidRequest'{errors = Errors} ->
%%                     {ok, {400, [], logic_error(invalidRequest, format_request_errors(Errors))}};
%%                 #payproc_InvalidPartyStatus{} ->
%%                     {ok, {400, [], logic_error(invalidPartyStatus, <<"Invalid party status">>)}};
%%                 #payproc_InvalidShopStatus{} ->
%%                     {ok, {400, [], logic_error(invalidShopStatus, <<"Invalid shop status">>)}};
%%                 #payproc_InvalidUser{} ->
%%                     {ok, {404, [], general_error(<<"Invoice not found">>)}};
%%                 #payproc_InvoiceNotFound{} ->
%%                     {ok, {404, [], general_error(<<"Invoice not found">>)}}
%%             end;
%%         {error, invalid_token} ->
%%             {ok, {400, [], logic_error(
%%                 invalidPaymentToolToken,
%%                 <<"Specified payment tool token is invalid">>
%%             )}};
%%         {error, invalid_payment_session} ->
%%             {ok, {400, [], logic_error(
%%                 invalidPaymentSession,
%%                 <<"Specified payment session is invalid">>
%%             )}}
%%     end;

%% process_request('CreateInvoiceAccessToken', Req, Context) ->
%%     InvoiceID = maps:get(invoiceID, Req),
%%     case get_invoice_by_id(InvoiceID, Context) of
%%         {ok, #'payproc_Invoice'{}} ->
%%             {ok, {201, [], issue_access_token(get_party_id(Context), {invoice, InvoiceID})}};
%%         {exception, Exception} ->
%%             case Exception of
%%                 #payproc_InvalidUser{} ->
%%                     {ok, {404, [], general_error(<<"Invoice not found">>)}};
%%                 #payproc_InvoiceNotFound{} ->
%%                     {ok, {404, [], general_error(<<"Invoice not found">>)}}
%%             end
%%     end;

%% validate_webhook_params(#webhooker_WebhookParams{event_filter = EventFilter}) ->
%%     validate_event_filter(EventFilter).

%% validate_event_filter({invoice, #webhooker_InvoiceEventFilter{shop_id = ShopID}}) ->
%%     validate_event_filter_shop(ShopID);

%% validate_event_filter({customer, #webhooker_CustomerEventFilter{shop_id = ShopID}}) ->
%%     validate_event_filter_shop(ShopID).

%% validate_event_filter_shop(ShopID) when ShopID /= undefined ->
%%     ShopID.

%% get_webhook(WebhookID, Context) ->
%%     PartyID = get_party_id(Context),
%%     case service_call({webhook_manager, 'Get', [WebhookID]}, Context) of
%%         {ok, Webhook = #webhooker_Webhook{party_id = PartyID}} ->
%%             {ok, Webhook};
%%         {ok, _Webhook} ->
%%             {exception, #webhooker_WebhookNotFound{}};
%%         {exception, Exception} ->
%%             {exception, Exception}
%%     end.

%% encode_webhook_id(WebhookID) ->
%%     try
%%         {ok, binary_to_integer(WebhookID)}
%%     catch
%%         error:badarg ->
%%             error
%%     end.

%% encode_webhook_params(PartyID, #{<<"scope">> := Scope, <<"url">> := URL}) ->
%%     #webhooker_WebhookParams{
%%         party_id     = PartyID,
%%         url          = URL,
%%         event_filter = encode_webhook_scope(Scope)
%%     }.

%% encode_webhook_scope(#{<<"topic">> := <<"InvoicesTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
%%     {invoice, #webhooker_InvoiceEventFilter{
%%         shop_id = ShopID,
%%         types   = ordsets:from_list([
%%             encode_invoice_event_type(V) || V <- EventTypes
%%         ])
%%     }};
%% encode_webhook_scope(#{<<"topic">> := <<"CustomersTopic">>, <<"shopID">> := ShopID, <<"eventTypes">> := EventTypes}) ->
%%     {customer, #webhooker_CustomerEventFilter{
%%         shop_id = ShopID,
%%         types   = ordsets:from_list([
%%             encode_customer_event_type(V) || V <- EventTypes
%%         ])
%%     }}.

%% %%%

%% % Нужно быть аккуратным с флагами их порядок влияет на порядок аргументов при вызове функций!
%% % обычно параметры идут в порядке [user_info, party_id, party_creation],
%% % но это зависит от damsel протокола
%% service_call_with(Flags, Call, Context) ->
%%     % реверс тут чтобы в флагах писать порядок аналогично вызову функций
%%     service_call_with_(lists:reverse(Flags), Call, Context).

%% service_call_with_([user_info|T], {ServiceName, Function, Args}, Context) ->
%%     service_call_with_(T, {ServiceName, Function, [get_user_info(Context) | Args]}, Context);
%% service_call_with_([party_id|T], {ServiceName, Function, Args}, Context) ->
%%     service_call_with_(T, {ServiceName, Function, [get_party_id(Context) | Args]}, Context);
%% service_call_with_([party_creation|T], Call, Context) ->
%%     case service_call_with_(T, Call, Context) of
%%         {exception, #payproc_PartyNotFound{}} ->
%%             _ = lager:info("Attempting to create a missing party"),
%%             CreateCall = {party_management, 'Create', [get_party_params(Context)]},
%%             case service_call_with([user_info, party_id], CreateCall, Context) of
%%                 {ok       , _                     } -> service_call_with_(T, Call, Context);
%%                 {exception, #payproc_PartyExists{}} -> service_call_with_(T, Call, Context);
%%                 Error                               -> Error
%%             end;
%%         Result ->
%%             Result
%%     end;
%% service_call_with_([], Call, Context) ->
%%     service_call(Call, Context).

%% service_call({ServiceName, Function, Args}, #{woody_context := WoodyContext}) ->
%%     wapi_woody_client:call_service(ServiceName, Function, Args, WoodyContext).

create_processing_context(SwaggerContext, WoodyContext) ->
    #{
        woody_context   => WoodyContext,
        swagger_context => SwaggerContext
    }.

create_woody_context(#{'X-Request-ID' := RequestID}, AuthContext) ->
    RpcID = #{trace_id := TraceID} = woody_context:new_rpc_id(genlib:to_binary(RequestID)),
    _ = lager:debug("Created TraceID:~p for RequestID:~p", [TraceID , RequestID]),
    woody_user_identity:put(collect_user_identity(AuthContext), woody_context:new(RpcID)).

collect_user_identity(AuthContext) ->
    genlib_map:compact(#{
        id       => wapi_auth:get_subject_id(AuthContext),
        realm    => ?REALM,
        email    => wapi_auth:get_claim(<<"email">>, AuthContext, undefined),
        username => wapi_auth:get_claim(<<"name">> , AuthContext, undefined)
    }).

%% logic_error(Code, Message) ->
%%     #{<<"code">> => genlib:to_binary(Code), <<"message">> => genlib:to_binary(Message)}.

%% limit_exceeded_error(Limit) ->
%%     logic_error(<<"limitExceeded">>, io_lib:format("Max limit: ~p", [Limit])).

general_error(Message) ->
    #{<<"message">> => genlib:to_binary(Message)}.

%% parse_exp_date(ExpDate) when is_binary(ExpDate) ->
%%     [Month, Year0] = binary:split(ExpDate, <<"/">>),
%%     Year = case genlib:to_int(Year0) of
%%         Y when Y < 100 ->
%%             2000 + Y;
%%         Y ->
%%             Y
%%     end,
%%     {genlib:to_int(Month), Year}.

%% get_user_info(Context) ->
%%     #payproc_UserInfo{
%%         id = get_party_id(Context),
%%         type = {external_user, #payproc_ExternalUser{}}
%%     }.

%% get_auth_context(#{swagger_context := #{auth_context := AuthContext}}) ->
%%     AuthContext.

%% get_party_id(Context) ->
%%     wapi_auth:get_subject_id(get_auth_context(Context)).

%% get_peer_info(#{swagger_context := #{peer := Peer}}) ->
%%     Peer.

%% get_time(Key, Req) ->
%%     case genlib_map:get(Key, Req) of
%%         Timestamp when is_binary(Timestamp) ->
%%             wapi_utils:to_universal_time(Timestamp);
%%         undefined ->
%%             undefined
%%     end.

%% get_split_interval(SplitSize, minute) -> SplitSize * 60;
%% get_split_interval(SplitSize, hour  ) -> get_split_interval(SplitSize, minute) * 60;
%% get_split_interval(SplitSize, day   ) -> get_split_interval(SplitSize, hour  ) * 24;
%% get_split_interval(SplitSize, week  ) -> get_split_interval(SplitSize, day   ) * 7;
%% get_split_interval(SplitSize, month ) -> get_split_interval(SplitSize, day   ) * 30;
%% get_split_interval(SplitSize, year  ) -> get_split_interval(SplitSize, day   ) * 365.

%% get_time_diff(From, To) ->
%%     {DateFrom, TimeFrom} = parse_rfc3339_datetime(From),
%%     {DateTo, TimeTo} = parse_rfc3339_datetime(To),
%%     UnixFrom = genlib_time:daytime_to_unixtime({DateFrom, TimeFrom}),
%%     UnixTo = genlib_time:daytime_to_unixtime({DateTo, TimeTo}),
%%     UnixTo - UnixFrom.

%% parse_rfc3339_datetime(DateTime) ->
%%     {ok, {DateFrom, TimeFrom, _, _}} = rfc3339:parse(DateTime),
%%     {DateFrom, TimeFrom}.

%% format_request_errors([]    ) -> <<>>;
%% format_request_errors(Errors) -> genlib_string:join(<<"\n">>, Errors).

process_woody_error(_Source, result_unexpected   , _Details) -> {error, reply_5xx(500)};
process_woody_error(_Source, resource_unavailable, _Details) -> {error, reply_5xx(503)};
process_woody_error(_Source, result_unknown      , _Details) -> {error, reply_5xx(504)}.

%% get_my_party(Context) ->
%%     Call = {party_management, 'Get', []},
%%     service_call_with([user_info, party_id, party_creation], Call, Context).

%% collect_events(Limit, After, GetterFun, DecodeFun, Context) ->
%%     collect_events([], Limit, After, GetterFun, DecodeFun, Context).

%% collect_events(Collected, 0, _, _, _, _) ->
%%     {ok, Collected};

%% collect_events(Collected0, Left, After, GetterFun, DecodeFun, Context) when Left > 0 ->
%%     case get_events(Left, After, GetterFun) of
%%         {ok, Events} ->
%%             Filtered = decode_and_filter_events(DecodeFun, Context, Events),
%%             Collected = Collected0 ++ Filtered,
%%             case length(Events) of
%%                 Left ->
%%                     collect_events(
%%                         Collected,
%%                         Left - length(Filtered),
%%                         get_last_event_id(Events),
%%                         GetterFun,
%%                         DecodeFun,
%%                         Context
%%                     );
%%                 N when N < Left ->
%%                     {ok, Collected}
%%             end;
%%         Error ->
%%             Error
%%     end.

%% decode_and_filter_events(DecodeFun, Context, Events) ->
%%     lists:foldr(
%%         fun(Event, Acc) ->
%%              case DecodeFun(Event, Context) of
%%                 {true, Ev} ->
%%                     [Ev|Acc];
%%                 false ->
%%                     Acc
%%             end
%%         end,
%%         [],
%%         Events
%%     ).

%% get_last_event_id(Events) ->
%%     #payproc_Event{
%%         id = ID
%%     } = lists:last(Events),
%%     ID.

%% get_events(Limit, After, GetterFun) ->
%%     EventRange = #'payproc_EventRange'{
%%         limit = Limit,
%%         'after' = After
%%     },
%%     GetterFun(EventRange).

reply_5xx(Code) when Code >= 500 andalso Code < 600 ->
    {Code, [], <<>>}.

%% put_card_data_to_cds(CardData, SessionData, Context) ->
%%     Call = {cds_storage, 'PutCardData', [CardData, SessionData]},
%%     case service_call(Call, Context) of
%%         {ok, #'PutCardDataResult'{session_id = SessionID, bank_card = BankCard}} ->
%%             {{bank_card, BankCard}, SessionID};
%%         {exception, Exception} ->
%%             case Exception of
%%                 #'InvalidCardData'{} ->
%%                     throw({ok, {400, [], logic_error(invalidRequest, <<"Card data is invalid">>)}});
%%                 #'KeyringLocked'{} ->
%%                     % TODO
%%                     % It's better for the cds to signal woody-level unavailability when the
%%                     % keyring is locked, isn't it? It could always mention keyring lock as a
%%                     % reason in a woody error definition.
%%                     throw({error, reply_5xx(503)})
%%             end
%%     end.

%% prepare_client_ip(Context) ->
%%     #{ip_address := IP} = get_peer_info(Context),
%%     genlib:to_binary(inet:ntoa(IP)).

%% merge_and_compact(M1, M2) ->
%%     genlib_map:compact(maps:merge(M1, M2)).

%% decode_optional(Arg, DecodeFun) when Arg /= undefined ->
%%     DecodeFun(Arg);
%% decode_optional(undefined, _) ->
%%     undefined.
