-module(rpgb_handle_zone).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2]).

-record(ctx, {hostport, session, map, layer, rec, mode}).

get_routes() ->
	[
		<<"/maps/:mapid/layers/:layerid/auras">>,
		<<"/maps/:mapid/layers/:layerid/auras/:zoneid">>,
		<<"/maps/:mapid/layers/:layerid/zones">>,
		<<"/maps/:mapid/layers/:layerid/zones/:zoneid">>,
		<<"/maps/:mapid/layers/:layerid/scenery">>,
		<<"/maps/:mapid/layers/:layerid/scenery/:zoneid">>
	].

init(_Protos, Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{Path, Req2} = cowboy_req:path(Req1),
	{MapId, Req3} = cowboy_req:binding(mapid, Req2),
	Map = try list_to_integer(binary_to_list(MapId)) of
		MapN ->
			case rpgb_data:get_by_id(rpgb_rec_battlemap, MapN) of
				{ok, MapRec} -> MapRec;
				{error, notfound} -> notfound
			end
	catch
		'ERROR':{badarg, _} ->
			notfound
	end,
	{LayerId, Req4} = cowboy_req:binding(layerid, Req3),
	Layer = try list_to_integer(binary_to_list(LayerId)) of
		LayerN ->
			case rpgb_data:get_by_id(rpgb_rec_layer, LayerN) of
				{ok, LayerRec} -> LayerRec;
				{error, notfound} -> notfound
			end
	catch
		'ERROR':{badarg, _} ->
			notfound
	end,
	PreBin = <<"/maps/", MapId/binary, "/layers/", LayerId/binary, "/">>,
	PreBinSize = size(PreBin),
	<<PreBin:PreBinSize/binary, RestPath/binary>> = Path,
	Mode2 = case RestPath of
		<<"zones", _/binary>> -> zone;
		<<"auras", _/binary>> -> aura;
		<<"scenery", _/binary>> -> scenery;
		_ -> error
	end,
	{ZoneAuraId, Req5} = cowboy_req:binding(zoneid, Req4),
	ZoneAuraId1 = case ZoneAuraId of
		undefined ->
			undefined;
		_->
			try list_to_integer(binary_to_list(ZoneAuraId)) of
				ZoneAuraIdN ->
					ZoneAuraIdN
			catch
				'ERROR':{badarg,_} ->
					notfound
		end
	end,
	ZoneAura = case {ZoneAuraId1, Mode2} of
		{_, error} ->
			notfound;
		{undefined, _} ->
			undefined;
		{notfound, _} ->
			notfound;
		_ ->
			case rpgb_data:search(rpgb_rec_zone, [{id, ZoneAuraId1},{type, Mode2}]) of
				{ok, []} ->
					notfound;
				{ok, [Rec | _]} ->
					Rec
			end
	end,
	{ok, Req5, #ctx{hostport = HostPort, session = Session, map = Map, layer = Layer, rec = ZoneAura, mode = Mode2}}.

allowed_methods(Req, #ctx{rec = Id} = Ctx) when is_atom(Id) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"POST">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"persona">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{map = notfound} = Ctx) ->
	{ok, Req2} = cowboy_req:reply(404, Req),
	{halt, Req2, Ctx};
forbidden(Req, #ctx{map = Map, session = Session, mode = zone} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Allowed = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id orelse
				lists:member(User#rpgb_rec_user.id, Map#rpgb_rec_battlemap.participant_ids),
	{not Allowed, Req, Ctx};
forbidden(Req, #ctx{map = Map, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	Allowed = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id orelse
		lists:member(User#rpgb_rec_user.id, Map#rpgb_rec_battlemap.participant_ids),
	{not Allowed, Req, Ctx}.

resource_exists(Req, #ctx{map = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{layer = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{rec = notfound} = Ctx) ->
	{false, Req, Ctx};
resource_exists(Req, #ctx{rec = undefined} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, Ctx) ->
	{true, Req, Ctx}.

delete_resource(Req, #ctx{rec = undefined} = Ctx) ->
	{false, Req, Ctx};
delete_resource(Req, Ctx) ->
	Layer = rpgb_rec_zone:delete(Ctx#ctx.rec, Ctx#ctx.layer),
	{true, Req, Ctx#ctx{layer = Layer}}.

content_types_provided(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, to_json}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, #ctx{rec = undefined} = Ctx) ->
	Layer = Ctx#ctx.layer,
	IdList = case Ctx#ctx.mode of
		zone ->
			Layer#rpgb_rec_layer.zone_ids;
		aura ->
			Layer#rpgb_rec_layer.aura_ids;
		scenery ->
			Layer#rpgb_rec_layer.scenery_ids
	end,
	Jsons = lists:map(fun(Id) ->
		{ok, Rec} = rpgb_data:get_by_ic(rpgb_rec_zone, Id),
		rpgb_rec_zone:make_json(Rec)
	end, IdList),
	{jsx:to_json(Jsons), Req, Ctx};

to_json(Req, #ctx{rec = Rec} = Ctx) ->
	Json = make_json(Ctx),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{rec = undefined} = Ctx) ->
	#ctx{map = Map, layer = Layer, mode = Mode} = Ctx,
	InitialZone = #rpgb_rec_zone{
		id = undefined, name = <<>>, layer_id = Layer#rpgb_rec_layer.id,
		type = Mode, created = os:timestamp(), updated = os:timestamp()
	},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case rpgb_rec_zone:validate(Term, InitialZone) of
		{ok, {_Json, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			{ok, Layer2} = insert_zone(Layer, Rec2),
			Ctx2 = Ctx#ctx{layer = Layer2, rec = Rec2},
			Location = make_location(Req1, Ctx2),
			{OutBody, Req3, Ctx3} = to_json(Req1, Ctx2),
			Req4 = cowboy_req:set_resp_body(OutBody, Req3),
			{{true, Location}, Req4, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{false, Req2, Ctx}
	end;

from_json(Req, Ctx) ->
	#ctx{map = Map, layer = Layer, mode = Mode, rec = InitRec} = Ctx,
	InitialZone = InitRec#rpgb_rec_zone{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case rpgb_rec_zone:validate(Term, InitialZone) of
		{ok, {_Json, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			Ctx2 = Ctx#ctx{rec = Rec2},
			{OutBody, Req3, Ctx3} = to_json(Req, Ctx2),
			Req4 = cowboy_req:set_resp_body(OutBody, Req3),
			{true, Req4, Ctx3};
		{error, Status, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{false, Req2, Ctx}
	end.

generate_etag(Req, #ctx{map = notfound} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{layer = notfound} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{rec = Atom} = Ctx) when is_atom(Atom) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{rec = Rec} = Ctx) ->
	Bin = jsx:to_json(Rec:to_json()),
	Updated = Rec#rpgb_rec_zone.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.

make_location(Req, Ctx) ->
	#ctx{hostport = {Host, Port}, map = Map, layer = Layer, rec = Rec, mode = Mode} = Ctx,
	ModeList = atom_to_list(Mode),
	rpgb:get_url(Req, Host, Port, ["maps", integer_to_list(Map#rpgb_rec_battlemap.id),
		"layers", integer_to_list(Layer#rpgb_rec_layer.id), ModeList ++ "s",
		integer_to_list(Rec#rpgb_rec_zone.id)]).

make_json(Ctx) ->
	#ctx{rec = Rec, map = Map} = Ctx,
	MapId = Map#rpgb_rec_battlemap.id,
	rpgb_rec_zone:make_json(Rec, MapId).

insert_zone(Layer, #rpgb_rec_zone{type = aura, id = Id}) ->
	Ids = Layer#rpgb_rec_layer.aura_ids ++ [Id],
	rpgb_data:save(Layer#rpgb_rec_layer{aura_ids = Ids});
insert_zone(Layer, #rpgb_rec_zone{type = zone, id = Id}) ->
	Ids = Layer#rpgb_rec_layer.zone_ids ++ [Id],
	rpgb_data:save(Layer#rpgb_rec_layer{zone_ids = Ids});
insert_zone(Layer, #rpgb_rec_zone{type = scenery, id = Id}) ->
	Ids = Layer#rpgb_rec_layer.scenery_ids ++ [Id],
	rpgb_data:save(Layer#rpgb_rec_layer{scenery_ids = Ids}).
