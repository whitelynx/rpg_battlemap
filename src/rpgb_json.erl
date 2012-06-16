%% @doc Does a best effort to convert boss_records to and from json.
%% uses mochijson2 to do the actual encode/decode.
%%
%% boss records can export 3 optional functions that control how this will
%% encode and decode.
%%
%% == json_enc_exclude() -> [atom()] ==
%% 
%% Exclude the listed attributes, has, or belongs_to keys from the json
%% when encoding.
%%
%% == json_dec_exclude() -> [atom()] ==
%%
%% Ignore the listed attributes or has when decoding from json.
%%
%% == json_prop_names() -> [{atom(), atom() | binary()}] ==
%%
%% Element 1 is how an attribute appears to the boss_record, eg:  
%% BossRec:Key1().  Element 2 is now the same attribute appears as a json
%% property name.  For example, [{foo_id, <<"fooId">>}].

-module(rpgb_json).
-export([to_json/1,from_json/2]).
% TODO these could stand to be in util.
-export([floor/1, ceiling/1]).

-include("log.hrl").

to_json(BossRec) ->
	to_json(BossRec, []).

to_json(BossRec, SkipRecs) ->
	Mod = boss_db:type(BossRec:id()),
	SkipRecs0 = [Mod | SkipRecs],

	Exclude = case erlang:function_exported(Mod, json_enc_exclude, 1) of
		true -> BossRec:json_enc_exclude();
		_ -> []
	end,
	Attr = BossRec:attributes(),
	Attr0 = [A || {Key, _Val} = A <- Attr, not lists:member(Key, Exclude)],

	TypesMap = BossRec:attribute_types(),
	Attr1 = correct_types(Attr0, TypesMap),

	Lexxed = case erlang:function_exported(Mod, json_prop_names, 1) of
		true -> BossRec:json_prop_names();
		_ -> []
	end,
	Attr2 = proplists:substitute_aliases(Lexxed, Attr1),

	BelongsToAttrs = assign_belongs(BossRec, SkipRecs0),
	Attr3 = lists:append(Attr2, BelongsToAttrs),
	Hases = assign_hases(BossRec, SkipRecs0),
	lists:append(Attr3, Hases).

assign_hases(BossRec, SkipRecs) when is_list(SkipRecs) ->
	case boss_db:type(BossRec:id()) of
		Err when Err == undefined; Err == error ->
			?info("Skipping has ~p as module is err:  ~p", [BossRec:id(), Err]),
			[];
		Mod ->
			ModAttr = Mod:module_info(attributes),
			HasAttrs = lists:flatten(proplists:get_all_values(has, ModAttr)),
			HasAttrs0 = filter_hases(HasAttrs, SkipRecs),
			[{element(1, H), assign_hases(BossRec, element(1,H), SkipRecs)} || H <- HasAttrs0]
	end.

assign_hases(BossRec, HasName, SkipRecs) when is_atom(HasName) ->
	Recs = BossRec:HasName(),
	[to_json(Rec, SkipRecs) || Rec <- Recs].

filter_hases(Hases, SkipRecs) ->
	filter_hases(Hases, SkipRecs, []).

filter_hases([], _, Acc) ->
	lists:reverse(Acc);

filter_hases([{Name, _Count} = H| Tail], Skips, Acc) ->
	case lists:member(Name, Skips) of
		true ->
			filter_hases(Tail, Skips, Acc);
		false ->
			filter_hases(Tail, Skips, [H | Acc])
	end;

filter_hases([{Name, _Count, Options} = H | Tail], Skips, Acc) ->
	NameMem = lists:member(Name, Skips),
	ModMem = lists:member(proplists:get_value(module, Options), Skips),
	if
		NameMem orelse ModMem ->
			filter_hases(Tail, Skips, Acc);
		true ->
			filter_hases(Tail, Skips, [H | Acc])
	end.


assign_belongs(BossRec, SkipRecs) when is_list(SkipRecs) ->
	Names = BossRec:belongs_to_names(),
	assign_belongs(Names, BossRec, SkipRecs).

assign_belongs(Names, BossRec, SkipRecs) ->
	assign_belongs(Names, BossRec, SkipRecs, []).

assign_belongs([], _BossRec, _SkipRecs, Acc) ->
	lists:reverse(Acc);

assign_belongs([Name | Tail], BossRec, SkipRecs, Acc) ->
	case BossRec:Name() of
		{error, Err} ->
			?info("not encoding ~s due to ~p", [Name, Err]),
			assign_belongs(Tail, BossRec, Acc);
		Rec ->
			Type = element(1, BossRec),
			case lists:member(Type, SkipRecs) of
				true ->
					assign_belongs(Tail, BossRec, Acc);
				false ->
					Json = to_json(Rec, SkipRecs),
					assign_belongs(Tail, BossRec, [{Name, Json} | Acc])
			end
	end.

correct_types(Attrs, Types) ->
	correct_types(Attrs, Types, []).

correct_types([], _Types, Acc) ->
	lists:reverse(Acc);

correct_types([{Key, Val} = H | Tail], Types, Acc) ->
	case {Val, proplists:get_value(Key, Types, string)} of
		{Val, string} when is_list(Val) ->
			correct_types(Tail, Types, [{Key, list_to_binary(Val)} | Acc]);
		{{Mega, Sec, Micro}, Time} when Time == timestamp; Time == datetime ->
			Val0 = (Mega * 1000000) + Sec + (Micro / 1000000),
			correct_types(Tail, Types, [{Key, Val0} | Acc]);
		{{{_Year, _Month, _Day},{_Hour, _Min, _Sec}}, Time} when Time == timestamp; Time == datetime ->
			Epoch = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
			ValEpoch = calendar:datetime_to_gregorian_seconds(Val),
			Diff = ValEpoch - Epoch,
			correct_types(Tail, Types, [{Key, Diff} | Acc]);
		{_, float} when is_float(Val) ->
			correct_types(Tail, Types, [H | Acc]);
		{_, integer} when is_integer(Val) ->
			correct_types(Tail, Types, [H | Acc]);
		{_, binary} when is_binary(Val) ->
			correct_types(Tail, Types, [H | Acc]);
		{_, boolean} when Val; not Val ->
			correct_types(Tail, Types, [H | Acc]);
		{_, DahType} ->
			?info("Skipping ~p: as value ~p isn't of type ~p.", [Key, Val, DahType]),
			correct_types(Tail, Types, Acc)
	end.

from_json(Binary, BossRec) when is_binary(Binary) ->
	case mochijson2:decode(Binary) of
		{struct, _Props} = Json ->
			from_json(Json, BossRec);
		Else ->
			{error, {badjson, Else}}
	end;

from_json(Json, RecType) when is_atom(RecType) ->
	BossRec = boss_record:new(RecType, [{id, id}]),
	from_json(Json, BossRec);

from_json({struct, Props}, BossRec) ->
	RecType = element(1, BossRec),
	Lexxed = case erlang:function_exported(RecType, json_prop_names, 1) of
		false -> [];
		true -> BossRec:json_prop_names()
	end,
	Lexxed0 = [{Val, Key} || {Key, Val} <- Lexxed],
	Props0 = proplists:substitute_aliases(Lexxed0, Props),
	AttrsTypes = BossRec:attribute_types(),
	AttrNames = BossRec:attribute_names(),
	Attrs = [{N, proplists:get_value(N, AttrsTypes, any)} || N <- AttrNames],
	%BelongsNames = [{B, belongs_to} || B <- BossRec:belongs_to_names()],
	HasTypes = extract_has_types(RecType),
	Excluded = case erlang:function_exported(RecType, json_dec_exclude, 1) of
		true -> BossRec:json_dec_exclude();
		_ -> []
	end,
	AllNames = lists:append([Attrs, HasTypes]),
	FilteredNames = [X || {Name, _} = X <- AllNames, not lists:member(Name, Excluded)],
	from_json(Props0, FilteredNames, BossRec).

from_json([], _Names, BossRec) ->
	{ok, BossRec};

from_json([{PropName, PropValue} | Tail], Names, BossRec) ->
	NameTypeList = [ X || {Name, _Type} = X <- Names, propname_match(PropName, Name)],
	case NameTypeList of
		[] ->
			?info("Skipping ~p as it's not in the list of settables", [PropName]),
			from_json(Tail, Names, BossRec);
		[{Name, Type} | _] ->
			case from_json({PropName, PropValue}, {Name, Type}, BossRec) of
				{ok, BossRec0} ->
					from_json(Tail, Names, BossRec0);
				{error, Error} ->
					{error, Error}
			end
	end;

from_json({_PropName, PropValue}, {Name, any}, BossRec) ->
	{ok, BossRec:set(Name, PropValue)};

from_json({_PropName, PropValue}, {Name, string}, BossRec) when is_binary(PropValue) ->
	{ok, BossRec:set(Name, binary_to_list(PropValue))};

from_json({_PropName, PropValue}, {Name, binary}, BossRec) when is_binary(PropValue) ->
	{ok, BossRec:set(Name, PropValue)};

from_json({_PropName, PropValue}, {Name, integer}, BossRec) when is_integer(PropValue) ->
	{ok, BossRec:set(Name, PropValue)};

from_json({_PropName, PropValue}, {Name, float}, BossRec) when is_integer(PropValue) ->
	{ok, BossRec:set(Name, PropValue + 0.0)};

from_json({_PropName, PropValue}, {Name, float}, BossRec) when is_float(PropValue) ->
	{ok, BossRec:set(Name, PropValue)};

from_json({_PropName, PropValue}, {Name, Timey}, BossRec) when is_number(PropValue), (Timey =:= timestamp orelse Timey =:= datetime) ->
	Mega = floor(PropValue / 1000000),
	Sec = floor(PropValue - (Mega * 1000000)),
	Micro = PropValue - ( (Mega * 1000000) + Sec),
	{ok, BossRec:set(Name, {Mega, Sec, Micro})};

from_json({_PropName, PropValue}, {Name, boolean}, BossRec) when PropValue; not PropValue ->
	{ok, BossRec:set(Name, PropValue)};

from_json({_PropName, PropValue}, {_Name, [RecType]}, BossRec) when is_list(PropValue) ->
	% boss_db/boss_record does magic under the hood to make sure the
	% BossRec:has_attr() gets the correct responses here.
	% so long as the belongs_to attr is set correctly in each sub object
	% so we'll do some checking to see if we can make them match.
	ParentRecType = element(1, BossRec),
	"rpgb_" ++ ParentRecStr = atom_to_list(ParentRecType),
	ParentRecAttrStr = ParentRecStr ++ "_id",
	BaseRec = boss_record:new(RecType, []),
	Attrs = BaseRec:attribute_names(),
	BaseRec0 = case [A || A <- Attrs, atom_to_list(A) =:= ParentRecAttrStr] of
		[] ->
			?info("Couldn't guess the belongs to assoc"),
			BaseRec;
		_ ->
			BaseRec:set(list_to_atom(ParentRecAttrStr), BossRec:id())
	end,
	SaveRes = [case from_json(Json, BaseRec0) of
		{ok, BaseRec1} ->
			BaseRec1:save();
		OhGod ->
			?info("Couldn't save sub thing:  ~p", [OhGod]),
			OhGod
	end || {struct, _Props} = Json <- PropValue],
	case lists:any(fun({ok, _}) -> false; (_) -> true end, SaveRes) of
		true ->
			{error, SaveRes};
		_ ->
			{ok, BossRec}
	end;

from_json(PropData, NameData, _BossRec) ->
	{error, {badjson, PropData, NameData}}.
	
	

floor(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T - 1;
        Pos when Pos > 0 -> T;
        _ -> T
    end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
		

propname_match(PropName, PropName) ->
	true;

propname_match(PropName, Name) when is_atom(Name) ->
	case list_to_binary(atom_to_list(Name)) of
		PropName -> true;
		_ -> false
	end;

propname_match(_PropName, _Name) ->
	false.

extract_has_types(RecType) when is_atom(RecType) ->
	ModAttr = RecType:module_info(attributes),
	Hases = proplists:get_all_values(has, ModAttr),
	Hases0 = lists:flatten(Hases),
	extract_has_types(Hases0);

extract_has_types(Hases) when is_list(Hases) ->
	extract_has_types(Hases, []).

extract_has_types([], Acc) ->
	lists:reverse(Acc);

extract_has_types([{Name, _Count} | Tail], Acc) ->
	extract_has_types(Tail, [{Name, [Name]} | Acc]);

extract_has_types([{Name, _Count, Options} | Tail], Acc) ->
	Module = proplists:get_value(module, Options, Name),
	extract_has_types(Tail, [{Name, [Module]} | Acc]).
