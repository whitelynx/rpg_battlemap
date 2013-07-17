-module(rpgb_rec_zone).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1, make_json/2]).
-export([delete/1]).
-export([validate/2]).

make_json(Zone) ->
	{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, Zone#rpgb_rec_zone.layer_id),
	make_json(Zone, Layer#rpgb_rec_layer.battlemap_id).

make_json(Zone, MapId) ->
	Path = iolist_to_binary(io_lib:format("maps/~p/layers/~p/~s/~p", [
		MapId, Zone#rpgb_rec_zone.layer_id, url_name(Zone#rpgb_rec_zone.type), Zone#rpgb_rec_zone.id])),
	Url = rpgb:get_url(Path),
	Zone:to_json([{url, Url}, type, {null_is_undefined}, fun fix_attrs/2]).

fix_attrs(Json, #rpgb_rec_zone{element_attrs = []}) ->
	lists:keystore(<<"element_attrs">>, 1, Json, {<<"element_attrs">>, [{}]});
fix_attrs(Json, _Rec) ->
	Json.

delete(#rpgb_rec_zone{layer_id = Lid} = Zone) ->
	case rpgb_data:get_by_id(rpgb_rec_layer, Lid) of
		{ok, Layer} ->
			delete(Zone, Layer);
		Else ->
			{error, {layer_lookup, Else}}
	end;

delete(Id) ->
	case rpgb_data:get_by_id(rpgb_rec_zone, Id) of
		{ok, Rec} ->
			delete(Rec);
		Else ->
			Else
	end.

delete(Zone, Layer) ->
	Layer2 = remove_from_layer(Zone, Layer),
	rpgb_data:delete(Zone),
	Layer2.

validate(Json, InitialZone) ->
	ValidateFuns = [
		fun rpgb_validation:scrub_disallowed/1,
		fun scrub_disallowed/1,
		fun rpgb_validation:check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json_warnings/1,
		fun check_named_zone/1
	],
	rpgb:bind({Json, InitialZone}, ValidateFuns).

scrub_disallowed({[{}], _Zone} = In) ->
	{ok, In};
scrub_disallowed({Json, Zone}) ->
	Disallowed = [<<"type">>],
	Json2 = [KV || {Key, _Value} = KV <- Json, not lists:member(Key, Disallowed)],
	{ok, {Json2, Zone}}.

check_name_conflict({Json, Zone} = In) ->
	ZoneName = #rpgb_rec_zone.name,
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, In};
		ZoneName when is_binary(ZoneName) ->
			{ok, In};
		Name ->
			#rpgb_rec_zone{type = Mode, layer_id = LayerId} = Zone,
			case rpgb_data:search(rpgb_rec_zone, [{type, Mode}, {layer_id, LayerId}, {name, Name}]) of
				{ok, []} ->
					{ok, In};
				_ ->
					{error, {conflict, <<"Name is already used on that layer">>}}
			end
	end.

validate_json_warnings({Json, Zone}) ->
	case Zone:from_json(Json, [null_is_undefined]) of
		{ok, Zone2} ->
			{ok, {Json, Zone2}};
		{ok, Zone2, Warnings} ->
			case validate_warnings(Zone2, Warnings) of
				{ok, Zone3} ->
					{ok, {Json, Zone3}};
				Warnings2 ->
					{error, {invalid, iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Warnings2]))}}
			end;
		{_, Else} ->
			{error, {invalid, iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else]))}}
	end.

validate_warnings(Zone, Warnings) ->
	validate_warnings(Zone, Warnings, []).

validate_warnings(Zone, [], []) ->
	{ok, Zone};
validate_warnings(Zone, [], Acc) ->
	lists:reverse(Acc);
validate_warnings(Zone, [fill_color | Tail], Acc) ->
	case rpgb_validation:is_valid_color(Zone#rpgb_rec_zone.fill_color) of
		true ->
			validate_warnings(Zone, Tail, Acc);
		false ->
			validate_warnings(Zone, Tail, [fill_color | Acc])
	end;
validate_warnings(Zone, [stroke_color | Tail], Acc) ->
	case rpgb_validation:is_valid_color(Zone#rpgb_rec_zone.stroke_color) of
		true ->
			validate_warnings(Zone, Tail, Acc);
		false ->
			validate_warnings(Zone, Tail, [stroke_color| Acc])
	end.

check_named_zone({Json, #rpgb_rec_zone{name = undefined}}) ->
	{error, {invalid, <<"name cannot be blank">>}};
check_named_zone(In) ->
	{ok, In}.


























url_name(aura) -> "auras";
url_name(zone) -> "zones";
url_name(scenery) -> "scenery".

remove_from_layer(#rpgb_rec_zone{id = Id, type = aura}, Layer) ->
	Ids = lists:delete(Id, Layer#rpgb_rec_layer.aura_ids),
	rpgb_data:save(Layer#rpgb_rec_layer{aura_ids = Ids});

remove_from_layer(#rpgb_rec_zone{id = Id, type = zone}, Layer) ->
	Ids = lists:delete(Id, Layer#rpgb_rec_layer.zone_ids),
	rpgb_data:save(Layer#rpgb_rec_layer{zone_ids = Ids});

remove_from_layer(#rpgb_rec_zone{id = Id, type = scenery}, Layer) ->
	Ids = lists:delete(Id, Layer#rpgb_rec_layer.scenery_ids),
	rpgb_data:save(Layer#rpgb_rec_layer{scenery_ids = Ids}).
