-module(roster).

-export([start/0, stop/0, handle_packet/1]).

-include_lib("exmpp/include/exmpp.hrl").

%% TODO: roster handling for better PEP support
%%-record(roster, {jid, subscription}).

start() ->
    %%mnesia:create_table(roster, [{attributes, record_info(fields, roster)}]),
    client:register_listener(?MODULE).

stop() ->
    client:unregister_listener(?MODULE).

handle_packet(#xmlel{name = presence} = Pkt) ->
    case exmpp_xml:get_attribute(Pkt, type, "") of
	"subscribe" ->
	    Reply = exmpp_stanza:set_type(
		      exmpp_stanza:reply_without_content(Pkt),
		      "subscribed"),
	    client:send(Reply);
	_ ->
	    ignored
    end;

handle_packet(_) ->
    ignored.
