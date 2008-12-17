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
	  exmpp_xml:get_element(Pkt, "body")} of
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
				  [{xmlelement, "body", [],
				    [{xmlcdata, Text}]},
				   {xmlelement, "html", [{"xmlns", ?NS_XHTML_IM_s}],
				    [{xmlelement, "body", [{"xmlns", ?NS_XHTML_s}],
				      HTML}]}];
			  Text when is_list(Text) ->
			      Children =
				  [{xmlelement, "body", [],
				    [{xmlcdata, Text}]}]
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

-define(UL(LIs), {xmlelement, "ul", [], LIs}).
-define(LI(Els), {xmlelement, "li", [], Els}).
-define(A(Href, Text), {xmlelement, "a", [{"href", Href}],
			[{xmlcdata, Text}]}).

handle_text(From, "list") ->
    Subscriptions = subscriptions:get_user_subscriptions(From),
    {"Subscriptions:\n" ++ [JID ++ " " ++ Node ++ "\n"
			    || {JID, Node} <- Subscriptions],
     [{xmlelement, "h3", [],
       [{xmlcdata, "Subscriptions"}]},
      {xmlelement, "table", [{"border", "1"}],
       [?UL([?LI([?A("xmpp:" ++ JID ++ "?pubsub", JID),
		  {xmlcdata, " "},
		  ?A("xmpp:" ++ JID ++ "?pubsub;node=" ++ Node, Node)])
	     || {JID, Node} <- Subscriptions])]}]};

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



