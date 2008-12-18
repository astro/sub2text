-module(sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
start_link(JID, Password) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [JID, Password]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([JID, Password]) ->
    Client = {client,{client,start_link,[JID, Password]},
	      permanent,2000,worker,[client,
				     pubsub, chat, roster]},
    {ok,{{one_for_all,0,1}, [Client]}}.

%%====================================================================
%% Internal functions
%%====================================================================
