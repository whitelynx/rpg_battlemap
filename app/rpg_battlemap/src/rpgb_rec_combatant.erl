-module(rpgb_rec_combatant).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1]).
-export([delete/1, delete/2, validate/1, validate/2]).
-export([append/1, append/2]).

make_json(Combatant) ->
	Url = rpgb:get_url(["map", 
		integer_to_list(Combatant#rpgb_rec_combatant.battlemap_id),
		"combatants", integer_to_list(Combatant#rpgb_rec_combatant.id)]),
	Combatant:to_json([{url, Url}]).

delete(#rpgb_rec_combatant{battlemap_id = MapId} = Combatant) ->
	case rpgb_data:get_by_id(rpgb_rec_battlemap, MapId) of
		{ok, Map} ->
			delete(Combatant, Map);
		Else ->
			{error, {map_lookup, Else}}
	end;

delete(Id) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, Id) of
		{ok, Combatant} ->
			delete(Combatant);
		Else ->
			Else
	end.

delete(#rpgb_rec_combatant{battlemap_id = MapId} = Combatant, #rpgb_rec_battlemap{id = MapId} = Map) ->
	Clist = lists:delete(Combatant#rpgb_rec_combatant.id, Map#rpgb_rec_battlemap.combatant_ids),
	Map2 = Map#rpgb_rec_battlemap{combatant_ids = Clist},
	MapOut = rpgb_data:save(Map2),
	rpgb_data:delete(Combatant),
	MapOut;

delete(C, M) ->
	{error, {lolwut, C, M}}.

append(#rpgb_rec_combatant{battlemap_id = MapId} = Combatant) ->
	case rpgb_data:get_by_id(rpgb_rec_battlemap, MapId) of
		{ok, Map} ->
			append(Combatant, Map);
		MapErr ->
			{error, {map_lookup, MapErr}}
	end;

append(Id) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, Id) of
		{ok, Combatant} ->
			append(Combatant);
		Else ->
			Else
	end.

append(#rpgb_rec_combatant{battlemap_id = MapId} = Combatant, #rpgb_rec_battlemap{id = MapId} = Map) ->
	{ok, Combatant2} = rpgb_data:save(Combatant),
	Clist = Map#rpgb_rec_battlemap.combatant_ids ++ [Combatant2#rpgb_rec_combatant.id],
	Map2 = Map#rpgb_rec_battlemap{combatant_ids = Clist},
	{ok, Map3} = rpgb_data:save(Map2),
	{ok, {Combatant2, Map3}};

append(C, M) ->
	{error, {lolwut, C, M}}.

validate(Json) ->
	validate(Json, #rpgb_rec_combatant{}).

validate(Json, Rec) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_named_combatant/1,
		fun check_blank_name/1,
		fun validate_json/1,
		fun check_valid_layer/1
	],
	rpgb:bind({Json, Rec}, ValidateFuns).

scrub_disallowed({Json, Rec}) ->
	Disallowed = [<<"id">>, <<"battlemap_id">>, <<"owner_id">>, <<"created">>, <<"updated">>],
	Json2 = rpgb:scrub_disallowed(Json, Disallowed),
	{ok, {Json2, Rec}}.

check_named_combatant({Json, Rec} = In) ->
	case {proplists:get_value(<<"name">>, Json), Rec#rpgb_rec_combatant.name} of
		{undefined, undefined} ->
			{error, {invalid, <<"name cannot be blank">>}};
		_ ->
			{ok, In}
	end.

check_blank_name({Json, _Rec} = In) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, {invalid, <<"name cannot be blank">>}};
		null ->
			{error, {invalid, <<"name cannot be blank">>}};
		_ ->
			{ok, In}
	end.

validate_json({Json, Rec}) ->
	case Rec:from_json(Json, [null_is_undefined]) of
		{ok, Rec2} ->
			{ok, {Json, Rec2}};
		{ok, Rec2, Warnings} ->
			case validate_warnings(Rec2, Warnings) of
				{ok, Rec3} ->
					{ok, {Json, Rec3}};
				{error, _Rec3, Warnings2} ->
					Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Warnings2])),
					{error, {invalid, Body}}
			end;
		{_, Else} ->
			Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else])),
			{error, {invalid, Body}}
	end.

validate_warnings(Rec, Warnings) ->
	validate_warnings(Rec, Warnings, []).

validate_warnings(Rec, [], []) ->
	{ok, Rec};
validate_warnings(Rec, [], StillBad) ->
	{error, Rec, lists:reverse(StillBad)};
validate_warnings(Rec, [aura_color | Tail], Acc) ->
	case is_valid_color(Rec#rpgb_rec_combatant.aura_color) of
		true ->
			validate_warnings(Rec, Tail, Acc);
		false ->
			validate_warnings(Rec, Tail, [aura_color | Acc])
	end;
validate_warnings(Rec, [color | Tail], Acc) ->
	case is_valid_color(Rec#rpgb_rec_combatant.color) of
		true ->
			validate_warnings(Rec, Tail, Acc);
		false ->
			validate_warnings(Rec, Tail, [color | Acc])
	end;
validate_warnings(Rec, [Head | Tail], Acc) ->
	validate_warnings(Rec, Tail, [Head | Acc]).

is_valid_color([_R, _G, _B] = RGB) ->
	lists:all(fun(E) -> E < 256 andalso 0 =< E end, RGB);
is_valid_color([R,B,G,A]) ->
	RGB = [R,G,B],
	AValid = ( 0 =< A andalso A =< 1 ),
	AValid andalso is_valid_color(RGB).

check_valid_layer({_Json, #rpgb_rec_combatant{layer_id = undefined}} = In) ->
	{ok, In};
check_valid_layer({_Json, Rec} = In) ->
	#rpgb_rec_combatant{layer_id = LayerId, battlemap_id = MapId} = Rec,
	case rpgb_data:search(rpgb_rec_layer, [{id, LayerId}, {battlemap_id, MapId}]) of
		{ok, []} ->
			{error, {invalid, <<"no such layer">>}};
		{ok, Layers} when length(Layers) >= 1 ->
			{ok, In}
	end.

