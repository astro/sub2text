-module(chat).

-export([start/0, stop/0, handle_packet/1]).

-include_lib("exmpp/include/exmpp.hrl").

start() ->
    client:register_listener(?MODULE).

stop() ->
    client:unregister_listener(?MODULE).

handle_packet(#xmlel{name = PktName} = Pkt) ->
    case {PktName,
	  exmpp_xml:get_attribute(Pkt, type, "normal"),
	  exmpp_xml:get_element(Pkt, body)} of
	{message, PktType, #xmlel{} = Body} when PktType =/= "error" ->
	    %% TODO: strip JID
	    [_ | _] = From = exmpp_xml:get_attribute(Pkt, from, ""),
	    BodyText = strip(
			 binary_to_list(
			   exmpp_xml:get_cdata(Body))),
	    io:format("Body: ~p~nBodyText: ~p~n",[Body,BodyText]),
	    client:composing(
	      From,
	      fun() ->
		      case handle_text(From, BodyText) of
			  {Text, HTML} ->
			      Children =
				  [#xmlel{name = body,
					  ns = ?NS_JABBER_CLIENT,
					  children = #xmlcdata{cdata = Text}},
				   #xmlel{name = html,
					  ns = ?NS_XHTML_IM,
					  children = [#xmlel{name = body,
							     ns = ?NS_XHTML,
							     children = HTML}]}];
			  Text when is_list(Text) ->
			      Children =
				  [#xmlel{name = body,
					  ns = ?NS_JABBER_CLIENT,
					  children = #xmlcdata{cdata = Text}}]
		      end,

		      client:send(
			exmpp_stanza:set_recipient(
			  (exmpp_message:chat())#xmlel{children = Children},
			  From)),
		      ok
	      end);
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

-define(UL(LIs), #xmlel{name = ul,
			ns = ?NS_XHTML,
			children = LIs}).
-define(LI(Els), #xmlel{name = li,
			ns = ?NS_XHTML,
			children = Els}).
-define(A(Href, Text), #xmlel{name = a,
			      ns = ?NS_XHTML,
			      attrs = [#xmlattr{name = href,
						value = Href}],
			      children = [#xmlcdata{cdata = Text}]}).

handle_text(From, "list") ->
    Subscriptions = subscriptions:get_user_subscriptions(From),
    Subscriptions2 = lists:sort(
		       fun({JID1, _}, {JID2, _}) when JID1 < JID2 ->
			       true;
			  ({JID, Node1}, {JID, Node2}) when Node1 < Node2 ->
			       true;
			  (_, _) ->
			       false
		       end, Subscriptions),
    {"Subscriptions:\n" ++ [JID ++ " " ++ Node ++ "\n"
			    || {JID, Node} <- Subscriptions2],
     [#xmlel{name = h3,
	     ns = ?NS_XHTML,
	     children = [#xmlcdata{cdata = "Subscriptions"}]},
      ?UL([?LI([?A("xmpp:" ++ JID ++ "?pubsub", JID),
		#xmlcdata{cdata = " "},
		?A("xmpp:" ++ JID ++ "?pubsub;node=" ++ Node, Node)])
	   || {JID, Node} <- Subscriptions2])]};

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



