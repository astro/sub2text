-module(item_to_msg).

-export([transform_items/3]).

-include_lib("exmpp/include/exmpp_xml.hrl").
-include_lib("exmpp/include/exmpp_nss.hrl").

transform_items(JID, Node, Items) ->
    {Texts, HTMLChildren} =
	lists:foldr(
	  fun(Item, {Texts, HTMLChildren}) ->
		  {Texts1, HTMLChildren1} = transform_item(Item),
		  {Texts1 ++ Texts, HTMLChildren1 ++ HTMLChildren}
	  end, {[], []}, Items),
    [#xmlel{name = body,
	    ns = ?NS_JABBER_CLIENT,
	    children =
	    [#xmlcdata{cdata = string:join(
				 ["Updates for " ++ JID ++ " " ++ Node
				  | Texts], "\n")}]},
     #xmlel{name = html,
	    ns = ?NS_XHTML_IM,
	    children =
	    [#xmlel{name = body,
		    ns = ?NS_XHTML,
		    children =
		    [#xmlel{name = h1,
			    ns = ?NS_XHTML,
			    children =
			    [#xmlcdata{cdata = "Updates for " ++ JID ++ " " ++ Node}]}
		     | HTMLChildren]}]}].
    

transform_item(#xmlel{name = item,
		      children = Children}) ->
    Children1 = [Child
		 || Child <- Children,
		    element(1, Child) =:= xmlel],
    Texts = lists:map(fun to_text/1, Children1),
    HTMLChildren = lists:flatten(lists:map(fun to_html/1, Children1)),
    {Texts, HTMLChildren}.

-define(NS_ATOM, 'http://www.w3.org/2005/Atom').

to_text(#xmlel{name = entry, ns = ?NS_ATOM} = Entry) ->
    {Title, Link} = atom_info(Entry),
    [Title ++
	if 
	    is_list(Link) -> ": " ++ Link;
	    true -> ""
	end];

to_text(El) ->
    %% TODO: pretty-print
    exmpp_xml:node_to_list(El, [], []).


to_html(#xmlel{name = entry, ns = ?NS_ATOM} = Entry) ->
    {Title, Link} = atom_info(Entry),
    Title1 = case Title of
		 [_ | _] -> Title;
		 _ when is_list(Link) -> Link;
		 _ -> "(Untitled)"
	     end,
    [#xmlel{name = h2,
	    ns = ?NS_ATOM,
	    children =
	    [if is_list(Link) -> #xmlel{name = a,
					attrs = [#xmlattr{name = href,
							  value = Link}],
					children =
					[#xmlcdata{cdata = Title1}]};
		true -> #xmlcdata{cdata = Title}
	     end]}
     | find_atom_content(Entry)];

to_html(El) ->
    %% TODO: pretty-print
    #xmlel{name = pre,
	   children =
	   [#xmlcdata{cdata = exmpp_xml:node_to_list(El, [], [])}]}.


atom_info(Entry) ->
    case exmpp_xml:get_element(Entry, title) of
	#xmlel{} = TitleEl ->
	    Title = binary_to_list(exmpp_xml:get_cdata(TitleEl));
	_ -> Title = ""
    end,
    Link = find_suitable_atom_link(Entry),
    {Title, Link}.

find_suitable_atom_link(El) ->
    Links = exmpp_xml:get_elements(El, link),
    LinksSorted = lists:sort(
		    fun(Link1, Link2) ->
			    Rel1 = exmpp_xml:get_attribute(Link1, rel, false),
			    Type1 = exmpp_xml:get_attribute(Link1, type, false),
			    Rel2 = exmpp_xml:get_attribute(Link2, rel, false),
			    Type2 = exmpp_xml:get_attribute(Link2, type, false),
			    compare([{Rel1, Rel2, ["alternate", false, "self"]},
				     {Type1, Type2, ["application/xhtml+xml", "text/html", "text/plain"]}])
		    end, Links),
    case LinksSorted of
	[Link | _] ->
	    exmpp_xml:get_attribute(Link, href, false);
	_ ->
	    false
    end.

find_atom_content(#xmlel{name = entry,
			 ns = ?NS_ATOM,
			 children = Els}) ->
    {_Score, Xhtml} = lists:foldl(fun find_atom_content1/2,
				{0, none}, Els),
    case Xhtml of
	none -> [];
	[_ | _] -> Xhtml;
        _ -> [Xhtml]
    end.
	    

find_atom_content1(#xmlel{name = Name,
			  ns = ?NS_ATOM,
			  children = [_ | _] = ElChildren} = El,
		   {PrevScore, PrevEls})
  when Name =:= summary;
       Name =:= content ->
    Type = exmpp_xml:get_attribute(El, type, "text"),

    Score = bool_to_integer(Type == "xhtml") * 3 +
	bool_to_integer(Type == "html") +
	bool_to_integer(Name == summary) * 2 +
	bool_to_integer(Name == content) * 4,
    {NewScore, NewEls} =
	case Type of
	    "html" ->
		Html = concat_binary([<<"<div>">>,
				      exmpp_xml:get_cdata_from_list(ElChildren),
				      <<"</div>">>]),
		case (catch exmpp_xml:parse_document(Html)) of
		    Error when element(1, Error) =:= xml_parser ->
			{PrevScore, PrevEls};
		    Parsed ->
			{Score, Parsed}
		end;
	    "xhtml" ->
		{Score, ElChildren};
	    "text" ->
		Text = exmpp_xml:get_cdata_from_list(ElChildren),
		{Score, #xmlcdata{cdata = Text}}
	end,
    if
	NewScore > PrevScore -> {NewScore, NewEls};
	true -> {PrevScore, PrevEls}
    end;

find_atom_content1(_, {PrevScore, PrevEls}) ->
    {PrevScore, PrevEls}.

bool_to_integer(false) -> 0;
bool_to_integer(true)  -> 1.

compare([]) ->
    false;
compare([{Val1, Val2, Preferred} | Vals]) ->
    case {index(Val1, Preferred),
	  index(Val2, Preferred)} of
	{I1, false} when is_integer(I1) ->
	    true;
	{I1, I2} when is_integer(I1),
		      is_integer(I2),
		      I1 < I2 ->
	    true;
	{I1, I2} when is_integer(I1),
		      is_integer(I2) ->
	    false;
	_ ->
	    compare(Vals)
    end.

index(E, L) ->
    index(E, L, 1).

index(_, [], _) ->
    false;
index(E, [E | _], N) ->
    N;
index(E, [_ | L], N) ->
    index(E, L, N + 1).
