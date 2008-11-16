-module(subscriptions).

-export([init/0, subscribe/3, unsubscribe/3, get_subscribers_of/2]).

-record(subscription, {user, jid_node}).
-record(pending_subscribed, {jid_node, requesters}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(subscription,
			[{disc_copies, [node()]},
			 {type, bag},
			 {attributes, record_info(fields, subscription)}]),
    mnesia:add_table_index(subscription, jid_node),
    mnesia:create_table(pending_subscribed,
			[{attributes, record_info(fields, pending_subscribed)}]),
    ok.

subscribe(User, JID, Node) ->
    case subscribe1(JID, Node) of
	ok ->
	    {atomic, _} =
		mnesia:transaction(
		  fun() ->
			  mnesia:write(#subscription{user = User,
						     jid_node = {JID, Node}})
		  end),
	    ok;
	Error -> Error
    end.

subscribe1(JID, Node) ->
    I = self(),
    Ref = make_ref(),
    F = fun() ->
		case mnesia:read({pending_subscribed, {JID, Node}}) of
		    [] ->
			mnesia:write(#pending_subscribed{jid_node = {JID, Node},
							 requesters = []}),
			run;
		    [#pending_subscribed{requesters = R} = P] ->
			mnesia:write(P#pending_subscribed{requesters = [{I, Ref} | R]}),
			wait
		end
	end,
    {atomic, Action} = mnesia:transaction(F),
    case Action of
	wait ->
	    receive
		{Ref, Result} -> Result
	    end;
	run ->
	    Result = pubsub:subscribe(JID, Node),

	    F2 = fun() ->
			 case mnesia:read({pending_subscribed, {JID, Node}}) of
			     [#pending_subscribed{requesters = R} = P] ->
				 mnesia:delete_object(P),
				 R
			 end
		 end,
	    {atomic, Requesters} = mnesia:transaction(F2),
	    lists:foreach(fun({Pid, RRef}) ->
				  Pid ! {RRef, Result}
			  end, Requesters),

	    Result
    end.

unsubscribe(User, JID, Node) ->
    {atomic, Action} =
	mnesia:transaction(
	  fun() ->
		  mnesia:delete_object(#subscription{user = User,
						     jid_node = {JID, Node}}),
		  case mnesia:select(subscription, [{#subscription{jid_node = {JID, Node},
								   _ = '_'},
						     [], ['$_']}], read, 1) of
		      '$end_of_table' -> unsubscribe;
		      _ -> keep
		  end
	  end),
    case Action of
	keep ->
	    ok;
	unsubscribe ->
	    pubsub:unsubscribe(JID, Node)
    end.
		      

get_subscribers_of(JID, Node) ->
    F = fun() ->
		mnesia:select(subscription,
			      [{#subscription{jid_node = {JID, Node},
					      _ = '_'},
				[], ['$_']}])
	end,
    {atomic, Subscriptions} = mnesia:transaction(F),
    lists:map(fun(#subscription{user = User}) ->
		      User
	      end, Subscriptions).
