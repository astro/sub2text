{application, sub2text,
 [{description,  "XMPP PubSub to text message bridge"},
  {id,           "sub2text"},
  {vsn,          "v0.1"},
  {modules,      [chat, client, item_to_msg, pubsub,
		  roster, sub2text, subscriptions]},
  {applications, [sasl]},
  {env,          [{jid, "collector@jabber.ccc.de"},
		  {password, "traversal"}]},
  {mod,          {sub2text_app, []}}
]}.

