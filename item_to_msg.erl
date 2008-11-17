-module(item_to_msg).

-export([transform_items/2]).

-include_lib("exmpp_xml.hrl").
-include_lib("exmpp_nss.hrl").

transform_items(JID, #xmlel{name = items} = Items) ->
    Node = exmpp_xml:get_attribute(Items, node, ""),
    {Texts, HTMLChildren} =
	lists:foldr(
	  fun(Item, {Texts, HTMLChildren}) ->
		  {Texts1, HTMLChildren1} = transform_item(Item),
		  {Texts1 ++ Texts, HTMLChildren1 ++ HTMLChildren}
	  end, {[], []}, exmpp_xml:get_elements(Items, item)),
    [{xmlelement, "body", [],
      [{xmlcdata, string:join(
		    ["Updates for " ++ JID ++ " " ++ Node
		     | Texts], "\n")}]},
     {xmlelement, "html", [{"xmlns", ?NS_XHTML_IM_s}],
      [{xmlelement, "body", [{"xmlns", ?NS_XHTML_s}],
	[{xmlelement, "h3", [],
	  [{xmlcdata, "Updates for " ++ JID ++ " " ++ Node}]}
	 | HTMLChildren]}]}].
    

transform_item(#xmlel{name = item,
		      children = Children}) ->
    Children1 = [Child
		 || Child <- Children,
		    element(1, Child) =:= xmlel],
    Texts = lists:map(fun to_text/1, Children1),
    HTMLChildren = lists:map(fun to_html/1, Children1),
    {Texts, HTMLChildren}.

-define(NS_ATOM_s, "http://www.w3.org/2005/Atom").

to_text(#xmlel{name = "entry", ns = ?NS_ATOM_s} = Entry) ->
    {Title, Link} = atom_info(Entry),
    [Title ++
	if 
	    is_list(Link) -> ": " ++ Link;
	    true -> ""
	end];

to_text(El) ->
    %% TODO: pretty-print
    exmpp_xml:node_to_list(El, [], []).


to_html(#xmlel{name = "entry", ns = ?NS_ATOM_s} = Entry) ->
    {Title, Link} = atom_info(Entry),
    Title1 = case Title of
		 [_ | _] -> Title;
		 _ when is_list(Link) -> Link;
		 _ -> "(Untitled)"
	     end,
    {xmlelement, "p", [],
     [if is_list(Link) -> {xmlelement, "a", [{"href", Link}],
			   [{xmlcdata, Title1}]};
	 true -> {xmlcdata, Title}
      end]};

to_html(El) ->
    %% TODO: pretty-print
    {xmlelement, "pre", [],
     [{xmlcdata, exmpp_xml:node_to_list(El, [], [])}]}.



atom_info(Entry) ->
    case exmpp_xml:get_element(Entry, "title") of
	#xmlel{} = TitleEl ->
	    Title = binary_to_list(exmpp_xml:get_cdata(TitleEl));
	_ -> Title = ""
    end,
    Link = find_suitable_atom_link(Entry),
    {Title, Link}.

find_suitable_atom_link(El) ->
    Links = exmpp_xml:get_elements(El, "link"),
    LinksSorted = lists:sort(
		    fun(Link1, Link2) ->
			    Rel1 = exmpp_xml:get_attribute(Link1, "rel", false),
			    Type1 = exmpp_xml:get_attribute(Link1, "type", false),
			    Rel2 = exmpp_xml:get_attribute(Link2, "rel", false),
			    Type2 = exmpp_xml:get_attribute(Link2, "type", false),
			    compare([{Rel1, Rel2, ["alternate", false, "self"]},
				     {Type1, Type2, ["application/xhtml+xml", "text/html", "text/plain"]}])
		    end, Links),
    case LinksSorted of
	[Link | _] ->
	    exmpp_xml:get_attribute(Link, "href", false);
	_ ->
	    false
    end.

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
