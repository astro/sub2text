-module(sub2text_app).


-behaviour(application).

-export([start/2]).


start(normal, _Args) ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    subscriptions:init(),
    {ok, JID} = application:get_env(jid),
    {ok, Password} = application:get_env(password),
    {ok, Pid} = client:start_link(JID, Password),
    pubsub:start(),
    chat:start(),
    roster:start(),
    {ok, Pid};

start(_, _) ->
    {error, badarg}.
