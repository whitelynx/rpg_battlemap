-module(rpgb_rec_layer).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1, make_json/4]).
%-export([get_map_layers/1]).
-export([delete/1, delete/2]).
-export([update_from_json/2]).

make_json(Layer) ->
	MapFun = fun(Id) ->
		{ok, Zone} = rpgb_data:get_by_id(rpgb_rec_zone, Id),
		Zone
	end,
	Zones = lists:map(MapFun, Layer#rpgb_rec_layer.zone_ids),
	Scenery = lists:map(MapFun, Layer#rpgb_rec_layer.scenery_ids),
	Auras = lists:map(MapFun, Layer#rpgb_rec_layer.aura_ids),
	make_json(Layer, Zones, Scenery, Auras).

make_json(Layer, Zones, Scenery, Auras) ->
	Url = rpgb:get_url(["maps", integer_to_list(Layer#rpgb_rec_layer.battlemap_id), "layers", integer_to_list(Layer#rpgb_rec_layer.id)]),
	ZoneJsons = [rpgb_rec_zone:make_json(Zone, Layer#rpgb_rec_layer.battlemap_id) || Zone <- Zones],
	AurasJsons = [rpgb_rec_zone:make_json(Aura, Layer#rpgb_rec_layer.battlemap_id) || Aura <- Auras],
	Layer:to_json([{url, Url}, {zones, ZoneJsons}, {auras, AurasJsons}, first_aura_id, first_zone_id]).

delete(#rpgb_rec_layer{id = Id} = Layer) ->
	case rpgb_data:get_by_id(rpgb_rec_battlemap, Layer#rpgb_rec_layer.battlemap_id) of
		{ok, Map} ->
			delete(Layer, Map);
		Else ->
			{error, {get_map, Else}}
	end;

delete(Id) ->
	case rpgb_data:get_by_id(rpgb_rec_layer, Id) of
		{ok, Layer} ->
			delete(Layer);
		Else ->
			Else
	end.

delete(#rpgb_rec_layer{battlemap_id = MapId}, #rpgb_rec_battlemap{id = MapId, layer_ids = [_]}) ->
	{error, last_layer};

delete(#rpgb_rec_layer{battlemap_id = MapId} = Layer, #rpgb_rec_battlemap{id = MapId} = Map) ->
	Layers = lists:delete(Layer#rpgb_rec_layer.id, Map#rpgb_rec_battlemap.layer_ids),
	{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{layer_ids = Layers}),
	rpgb_data:delete(Layer),
	move_combatants(Layer),
	delete_zones(Layer),
	{ok, Map2}.

update_from_json(Json, InitLayer) when is_integer(InitLayer) ->
	case rpgb_data:get_by_id(?MODULE, InitLayer) of
		{ok, Layer} ->
			update_from_json(Json, Layer);
		Else ->
			Else
	end;

update_from_json(Json, InitLayer) ->
	case validate_layer(Json, InitLayer) of
		{ok, {_Json2, Rec}} ->
			{ok, Rec};
		Else ->
			Else
	end.

validate_layer(Json, InitLayer) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_blank_name/1,
		%fun check_name_conflict/1,
		fun validate_json/1,
		fun check_named_layer/1
	],
	rpgb:bind({Json, InitLayer}, ValidateFuns).

check_named_layer({Json, Layer}) ->
	LayerName = Layer#rpgb_rec_layer.name,
	JsonName = proplists:get_value(<<"name">>, Json),
	case {LayerName, JsonName} of
		{undefined, undefined} ->
			{error, {invalid, <<"name cannot be blank">>}};
		_ ->
			{ok, {Json, Layer}}
	end.

check_blank_name({Json, Layer}) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, {invalid, <<"name cannot be blank.">>}};
		null ->
			{error, {invalid, <<"name cannot be blank.">>}};
		_ ->
			{ok, {Json, Layer}}
	end.

%check_name_conflict({Json, Layer}) ->
%	#rpgb_rec_layer{battlemap_id = MapId, name = LayerName} = Layer,
%	case proplists:get_value(<<"name">>, Json) of
%		undefined ->
%			{ok, {Json, Layer}};
%		LayerName ->
%			{ok, {Json, Layer}};
%		OtherName ->
%			Searched = rpgb_data:search(rpgb_rec_layer, [
%				{name, OtherName}, {battlemap_id, MapId}]),
%			case Searched of
%				{ok, []} ->
%					{ok, {Json, Layer}};
%				_ ->
%					{error, 409, <<"you already have a layer by that name.">>}
%			end
%	end.

scrub_disallowed({Json, Map}) ->
	{ok, Json2} = scrub_disallowed(Json),
	{ok, {Json2, Map}};

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"battlemap_id">>, <<"created">>, <<"updated">>],
	Disallowed1 = ordsets:from_list(Disallowed),
	Json1 = ordsets:from_list(Json),
	scrub_disallowed(Json1, Disallowed1).

scrub_disallowed(Json, []) ->
	{ok, Json};

scrub_disallowed(Json, [Nope | Tail] = Nopes) ->
	case proplists:delete(Nope, Json) of
		Json ->
			scrub_disallowed(Json, Tail);
		Json1 ->
			scrub_disallowed(Json1, Nopes)
	end.

validate_json({Json, Layer}) ->
	case Layer:from_json(Json) of
		{ok, Layer2} ->
			{ok, {Json, Layer2}};
		{_, Else} ->
			Body = iolist_to_binary(io_lib:format("There were errors in the submitted json: ~p", [Else])),
			{error, {invalid, Body}}
	end.
	
move_combatants(#rpgb_rec_layer{id = Id}) ->
	{ok, Combatants} = rpgb_data:search(rpgb_rec_combatant, [{layer_id, Id}]),
	lists:map(fun(C) ->
		rpgb_data:save(C#rpgb_rec_combatant{layer_id = undefined})
	end, Combatants).

delete_zones(#rpgb_rec_layer{id = Id}) ->
	{ok, Zones} = rpgb_data:search(rpgb_rec_zone, [{layer_id, Id}]),
	lists:map(fun(Z) ->
		rpgb_data:delete(Z)
	end, Zones).