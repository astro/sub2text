-module(pubsub).

-export([start/0, stop/0, handle_packet/1, subscribe/2, unsubscribe/2]).

-include_lib("exmpp/include/exmpp.hrl").

-record(seen_item, {jni, last}).

start() ->
    mnesia:create_table(seen_item, [{disc_copies, [node()]},
				    {attributes, record_info(fields, seen_item)}]),
    client:register_listener(?MODULE).

stop() ->
    client:unregister_listener(?MODULE).

handle_packet(#xmlel{name = PktName} = Pkt) ->
    case {PktName,
	  exmpp_xml:get_attribute(Pkt, type, "normal"),
	  exmpp_xml:get_element(Pkt, event)} of
	{message, PktType,
	 #xmlel{ns = ?NS_PUBSUB_EVENT,
		children = Children}} when PktType =/= "error" ->
	    From = exmpp_xml:get_attribute(Pkt, from, ""),
	    handle_event(From, Children);
	_ ->
	    ignored
    end.

handle_event(_, []) ->
     ok;
handle_event(JID, [#xmlel{name = items} = Items | Els]) ->
    Node = exmpp_xml:get_attribute(Items, node, ""),
    {atomic, NewItems} =
	mnesia:transaction(
	  fun() ->
		  mnesia:write_lock_table(seen_item),

		  lists:filter(
		    fun(Item) ->
			    Id = exmpp_xml:get_attribute(Item, id, ""),
			    JNI = {JID, Node, Id},
			    case mnesia:read({seen_item, JNI}) of
				[] ->
				    mnesia:write(#seen_item{jni = JNI, last = current_timestamp()}),
				    true;
				_ ->
				    io:format("Skipping ~p~n", [JNI]),
				    false
			    end
		    end, exmpp_xml:get_elements(Items, item))
	  end),
    if
	NewItems =:= [] ->
	    ignore;
	true ->
	    case subscriptions:get_subscribers_of(JID, Node) of
		
		[] ->
		    unsubscribe(JID, Node);
		
		Users ->
		    Msg1 = exmpp_message:chat(),
		    Msg2 = Msg1#xmlel{children =
				      item_to_msg:transform_items(JID, Node, NewItems)},
		    lists:foreach(
		      fun(User) ->
			      client:send(exmpp_stanza:set_recipient(Msg2,
								     User))
		      end, Users)
	    end
    end,
    handle_event(JID, Els);
handle_event(JID, [_ | Els]) ->
    handle_event(JID, Els).

subscribe(JID, Node) ->
    case (catch subscribe1(JID, Node)) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("Error subscribing to ~p ~p:~n~p~n",
				   [JID, Node, Reason]),
	    error;
	ok -> ok;
	E ->
	    error_logger:error_msg("subscribe error: ~p~n", [E]),
	    E
    end.

subscribe1(JID, Node) ->
    Iq =
	#xmlel{name = iq,
	       ns = ?NS_JABBER_CLIENT,
	       attrs = [#xmlattr{name = to, value = JID},
			#xmlattr{name = type, value = "set"}],
	       children =
	       [#xmlel{name = pubsub,
		       ns = ?NS_PUBSUB,
		       children =
		       [#xmlel{name = subscribe,
			       ns = ?NS_PUBSUB,
			       attrs = [#xmlattr{name = node,
						 value = Node},
					#xmlattr{name = jid,
						 value = client:get_jid()}]}]}]},
    Answer = #xmlel{name = iq} = client:send_recv(Iq),
    case exmpp_xml:get_attribute(Answer, type, "") of
	"result" -> ok;
	"error" ->
	    Error = exmpp_xml:get_element(Answer, error),
	    What = exmpp_xml:get_element_by_ns(Error, ?NS_STANZA_ERRORS),
	    What#xmlel.name
    end.

unsubscribe(JID, Node) ->
    Iq =
	#xmlel{name = iq,
	       ns = ?NS_JABBER_CLIENT,
	       attrs = [#xmlattr{name = to, value = JID},
			#xmlattr{name = type, value = "set"}],
	       children =
	       [#xmlel{name = pubsub,
		       ns = ?NS_PUBSUB,
		       children =
		       [#xmlel{name = unsubscribe,
			       ns = ?NS_PUBSUB,
			       attrs = [#xmlattr{name = node,
						 value = Node},
					#xmlattr{name = jid,
						 value = client:get_jid()}]}]}]},
    client:send_recv(Iq),
    ok.

current_timestamp() ->
    {M, S, _} = now(),
    M * 1000000 + S.
