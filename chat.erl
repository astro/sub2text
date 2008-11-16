-module(chat).

-export([start/0, stop/0, handle_packet/1]).

-include_lib("exmpp.hrl").

start() ->
    client:register_listener(?MODULE).

stop() ->
    client:unregister_listener(?MODULE).

handle_packet(#xmlel{name = PktName} = Pkt) ->
    case {PktName,
	  exmpp_xml:get_attribute(Pkt, type, "normal"),
	  exmpp_xml:get_element(Pkt, "body")} of
	{message, PktType, #xmlel{} = Body} when PktType =/= "error" ->
	    [_ | _] = From = exmpp_xml:get_attribute(Pkt, from, ""),
	    BodyText = strip(
			 binary_to_list(
			   exmpp_xml:get_cdata(Body))),
	    io:format("Body: ~p~nBodyText: ~p~n",[Body,BodyText]),
	    Reply = handle_text(From, BodyText),
	    client:send(
	      exmpp_stanza:set_recipient(
		exmpp_message:chat(Reply),
		From)),
	    ok;
	_ ->
	    ignored
    end.

strip(S) ->
    WhitespaceDropper = fun(Ch) ->
				lists:member(Ch, "\t\r\n ")
			end,
    S2 = lists:dropwhile(WhitespaceDropper, S),
    S3 = lists:reverse(S2),
    S4 = lists:dropwhile(WhitespaceDropper, S3),
    S5 = lists:reverse(S4),
    S5.

handle_text(From, "subscribe " ++ JID_Node) ->
    [JID | NodeParts] = string:tokens(JID_Node, " "),
    Node = string:join(NodeParts, $ ),
    case subscriptions:subscribe(From, JID, Node) of
	ok -> "Ok, you will receive updates from " ++ Node;
	What -> io_lib:format("Error: ~s", [What])
    end;

handle_text(From, "unsubscribe " ++ JID_Node) ->
    [JID | NodeParts] = string:tokens(JID_Node, " "),
    Node = string:join(NodeParts, $ ),
    subscriptions:unsubscribe(From, JID, Node),
    "Ok, you will not receive updates from " ++ Node ++ " any longer";

handle_text(_From, "help") ->
    "Commands:\n"
	"subscribe <jid> <node>\n"
	"unsubscribe <jid> <node>\n"
	"unsubscribeall\n"
	"list\n";

handle_text(_From, _) ->
    "I did not understand. Try «help»".



