-module(sub2text_app).


-behaviour(application).

-export([start/0, start/2]).

start() ->
    application:start(sasl),
    application:start(exmpp),
    mnesia:create_schema([node()]),
    application:start(mnesia),
    application:start(sub2text).

start(normal, _Args) ->
    subscriptions:init(),
    {ok, JID} = application:get_env(jid),
    {ok, Password} = application:get_env(password),
    {ok, Pid} = sup:start_link(JID, Password),
    pubsub:start(),
    chat:start(),
    roster:start(),
    {ok, Pid};

start(_, _) ->
    {error, badarg}.
