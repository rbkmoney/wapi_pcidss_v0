-module(wapi_swagger_server).

-export([child_spec/1]).

-define(APP, wapi).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8080).

-type params() :: {cowboy_router:routes(), #{atom() => module()}}.

-spec child_spec(params()) ->
    supervisor:child_spec().
child_spec({HealthRoutes, LogicHandlers}) ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(HealthRoutes, LogicHandlers),
    CowboyOpts1 = cowboy_access_log_h:set_extra_info_fun(mk_operation_id_getter(CowboyOpts), CowboyOpts),
    ranch:child_spec(
        ?MODULE,
        Transport,
        TransportOpts,
        cowboy_clear,
        CowboyOpts1
    ).

get_socket_transport() ->
    {ok, IP}      = inet:parse_address(genlib_app:env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port          = genlib_app:env(?APP, port, ?DEFAULT_PORT),
    AcceptorsPool = genlib_app:env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    {ranch_tcp, [{ip, IP}, {port, Port}, {num_acceptors, AcceptorsPool}]}.

get_cowboy_config(HealthRoutes, LogicHandlers) ->
    Dispatch =
        cowboy_router:compile(squash_routes(
            HealthRoutes ++
            %% swag_server_wallet_router:get_paths(maps:get(wallet, LogicHandlers)) ++
            swag_server_payres_router:get_paths(maps:get(payres, LogicHandlers)) ++
            swag_server_privdoc_router:get_paths(maps:get(privdoc, LogicHandlers))
        )),
    #{
        env => #{
            dispatch => Dispatch,
            cors_policy => wapi_cors_policy
        },
        middlewares => [
            cowboy_router,
            cowboy_cors,
            cowboy_handler
        ],
        stream_handlers => [cowboy_access_log_h, wapi_stream_h, cowboy_stream_h]
    }.

squash_routes(Routes) ->
    orddict:to_list(lists:foldl(
        fun ({K, V}, D) -> orddict:update(K, fun (V0) -> V0 ++ V end, V, D) end,
        orddict:new(),
        Routes
    )).

mk_operation_id_getter(#{env := Env}) ->
    fun (Req) ->
        case cowboy_router:execute(Req, Env) of
            {ok, _, #{handler_opts := {_Operations, _LogicHandler, _SwaggerHandlerOpts} = HandlerOpts}} ->
                case swag_server_utils:get_operation_id(Req, HandlerOpts) of
                    undefined ->
                        #{};
                    OperationID ->
                        #{operation_id => OperationID}
                end;
            _ ->
                #{}
        end
    end.