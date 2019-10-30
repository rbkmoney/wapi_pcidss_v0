-module(wapi_ct_helper).

-include_lib("wapi_dummy_data.hrl").

-export([start_app/1]).
-export([start_app/2]).

-export([issue_token/2]).

%%

-type app_name() :: atom().

-spec start_app(app_name()) ->
    [app_name()].

start_app(woody = AppName) ->
    start_app(AppName, [
        {acceptors_pool_size, 4}
    ]);

start_app(scoper = AppName) ->
    start_app(AppName, [
        {storage, scoper_storage_logger}
    ]);

start_app(AppName) ->
    genlib_app:start_application(AppName).

-spec start_app(app_name(), list()) ->
    [app_name()].

start_app(AppName, Env) ->
    genlib_app:start_application_with(AppName, Env).

-spec issue_token( _, _) ->
    {ok, binary()} |
    {error,
        nonexistent_signee
    }.

issue_token(ACL, _LifeTime) ->
    wapi_auth:issue_access_token(?STRING, ACL).
