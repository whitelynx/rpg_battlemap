-module(rpgb_handle_layer).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2
]).

-record(ctx, { hostport, session, mapid, map, layerid, layer}).

get_routes() ->
	[
		<<"/maps/:mapid/layers">>,
		<<"/maps/:mapid/layers/:layerid">>
	].

init(_Protos, _Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	%?debug("Session:  ~p", [Session]),
	%{Path, Req2} = cowboy_req:path(Req1),
	{MapId, Req3} = cowboy_req:binding(mapid, Req1),
	MapId1 = case MapId of
		undefined ->
			undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				MapN ->
					MapN
			catch
				error:badarg ->
					undefined
			end
	end,
	{LayerId, Req4} = cowboy_req:binding(layerid, Req3),
	LayerId1 = case LayerId of
		undefined ->
			maplayers;
		_ ->
			try list_to_integer(binary_to_list(LayerId)) of
				LayerN -> LayerN
			catch
				error:badarg ->
					undefined
			end
	end,
	{ok, Req4, #ctx{hostport = HostPort, session = Session, mapid = MapId1, layerid = LayerId1}}.

allowed_methods(Req, #ctx{layerid = LayerId} = Ctx) when is_atom(LayerId) ->
	{[<<"GET">>, <<"POST">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"post">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{mapid = MapId, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	case rpgb_data:get_by_id(rpgb_rec_battlemap, MapId) of
		{error, notfound} ->
			{ok, Req2} = cowboy_req:reply(404, Req),
			{halt, Req2, Ctx};
		{ok, Map} ->
			{Mode, Req1} = cowboy_req:method(Req),
			UserIsOwner = User#rpgb_rec_user.id == Map#rpgb_rec_battlemap.owner_id,
			UserIsParticiapnt = rpgb_rec_battlemap:is_user_participant(User, Map),
			case {Mode, UserIsOwner, UserIsParticiapnt} of
				{_, true, _} ->
					{false, Req1, Ctx#ctx{map = Map}};
				{<<"GET">>, _, true} ->
					{false, Req1, Ctx#ctx{map = Map}};
				_ ->
					{true, Req1, Ctx#ctx{map = Map}}
			end
	end.

resource_exists(Req, #ctx{layerid = maplayers} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, #ctx{layerid = LayerId} = Ctx) ->
	{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, LayerId),
	{true, Req, Ctx#ctx{layer = Layer}}.

delete_resource(Req, #ctx{mapid = MapId} = Ctx) ->
	#ctx{map = Map, layerid = LayerId, layer = Layer} = Ctx,
	case rpgb_rec_layer:delete(Layer, Map) of
		{ok, Map2} ->
			{true, Req, Ctx};
		{error, last_layer} ->
			Req2 = cowboy_req:set_resp_body(<<"you cannot delete the last layer of a map">>, Req),
			{ok, Req3} = cowboy_req:reply(422, Req2),
			{halt, Req3, Ctx};
		{error, Wut} ->
			Req2 = cowboy_req:set_resp_body(io_lib:format("unknown error: ~p", [Wut])),
			{ok, Req3} = cowboy_req:reply(500, Req2),
			{halt, Req3, Ctx}
	end.

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

to_json(Req, #ctx{layerid = maplayers} = Ctx) ->
	#ctx{map = MapRec} = Ctx,
	Layers = [rpgb_data:get_by_id(rpgb_rec_layer, Id) || Id <- MapRec#rpgb_rec_battlemap.layer_ids],
	Json = [make_json(Req, Ctx, Layer) || {ok, Layer} <- Layers],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{layer = Layer} = Ctx) ->
	Json = make_json(Req, Ctx, Layer),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{layerid = maplayers} = Ctx) ->
	#ctx{map = Map, mapid = MapId} = Ctx,
	InitialLayer = #rpgb_rec_layer{
		id = undefined, name = <<>>, battlemap_id = MapId, created = os:timestamp(),
		updated = os:timestamp()
	},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case rpgb_rec_layer:update_from_json(Term, InitialLayer) of
		{ok, Rec} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			MapLayerList = Map#rpgb_rec_battlemap.layer_ids ++ [Rec2#rpgb_rec_layer.id],
			{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{layer_ids = MapLayerList}),
			Ctx2 = Ctx#ctx{layer = Rec2, layerid = Rec2#rpgb_rec_layer.id, map = Map2},
			Location = make_location(Req1, Ctx2, Rec2),
			{{true, Location}, Req1, Ctx2};
		{error, {invalid, Msg}} ->
			Req2 = cowboy_req:set_resp_body(jsx:to_json(Msg), Req1),
			{false, Req2, Ctx};
		{error, {conflict, Msg}} ->
			Req2 = cowboy_req:set_resp_body(jsx:to_json(Msg), Req1),
			Req3 = cowboy_req:reply(422, Req2),
			{halt, Req3, Ctx}
	end;

from_json(Req, #ctx{map = Map, layer = InitL} = Ctx) ->
	InitialLayer = InitL#rpgb_rec_layer{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	?debug("Submitted json:  ~p", [Term]),
	case rpgb_rec_layer:update_from_json(Term, InitialLayer) of
		{ok, Rec} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			Ctx2 = Ctx#ctx{layer = Rec2, layerid = Rec2#rpgb_rec_layer.id},
			Req2 = cowboy_req:set_resp_body(jsx:to_json(rpgb_rec_layer:make_json(Rec2)), Req1),
			{true, Req2, Ctx};
		{error, {invalid, Msg}} ->
			Req2 = cowboy_req:set_resp_body(jsx:to_json(Msg), Req1),
			{false, Req2, Msg};
		{error, {conflict, Msg}} ->
			Req2 = cowboy_req:set_resp_body(jsx:to_json(Msg), Req1),
			Req3 = cowboy_req:reply(422, Req2),
			{halt, Req3, Ctx}
	end.

make_json(_Req, _Ctx, Layer) ->
	rpgb_rec_layer:make_json(Layer).

make_location(_Req, _Ctx, Rec) ->
	rpgb:get_url(["maps", integer_to_list(Rec#rpgb_rec_layer.battlemap_id), "layers", integer_to_list(Rec#rpgb_rec_layer.id)]).

generate_etag(Req, #ctx{mapid = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{map = Map} = Ctx) ->
	Bin = jsx:to_json(Map:to_json()),
	Updated = Map#rpgb_rec_battlemap.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.
