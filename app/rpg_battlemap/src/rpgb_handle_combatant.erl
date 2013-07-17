-module(rpgb_handle_combatant).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).
-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, resource_exists/2,
	content_types_accepted/2, from_json/2, delete_resource/2,
	generate_etag/2
]).

-record(ctx, { hostport, session, mapid, map, combatantid, combatant, layerid}).

get_routes() ->
	[
		<<"/maps/:mapid/combatants">>,
		<<"/maps/:mapid/combatants/:combatantid">>,
		<<"/maps/:mapid/layers/:layerid/combatants">>
	].

init(_Protos, _Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{_Path, Req2} = cowboy_req:path(Req1),
	{MapId, Req3} = cowboy_req:binding(mapid, Req2),
	MapId1 = case MapId of
		undefined ->
			undefined;
		_ ->
			try list_to_integer(binary_to_list(MapId)) of
				MapN ->
					MapN
			catch
				'error':badarg ->
					undefined
			end
	end,
	{CombatantId, Req4} = cowboy_req:binding(combatantid, Req3),
	CombatantId1 = case CombatantId of
		undefined ->
			mapcombatants;
		_ ->
			try list_to_integer(binary_to_list(CombatantId)) of
				CombatantN -> CombatantN
			catch
				'error':badarg ->
					undefined
			end
	end,
	{LayerId, Req5} = cowboy_req:binding(layerid, Req4),
	{ok, Req5, #ctx{hostport = HostPort, session = Session, mapid = MapId1, combatantid = CombatantId1, layerid = LayerId}}.

%% ===============================
%% restful steps
%% ===============================

allowed_methods(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	{[<<"GET">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	{[<<"GET">>, <<"POST">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"persona">>}, Req, Ctx};
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
			Out = rpgb_rec_battlemap:is_user_participant(User, Map),
			{not Out, Req, Ctx#ctx{map = Map}}
	end.

resource_exists(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	#ctx{mapid = MapId} = Ctx,
	case rpgb_data:search(rpgb_rec_layer, [{battlemap_id, MapId},{id,LayerId}]) of
		{ok, []} ->
			{false, Req, Ctx};
		_ ->
			{true, Req, Ctx}
	end;
resource_exists(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	case cowboy_req:method(Req) of
		{<<"POST">>, Req2} ->
			{false, Req2, Ctx};
		{_, Req2} ->
			{true, Req2, Ctx}
	end;
resource_exists(Req, #ctx{combatantid = CombatantId} = Ctx) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, CombatantId) of
		{ok, Combatant} ->
			{true, Req, Ctx#ctx{combatant = Combatant}};
		{error, notfound} ->
			{false, Req, Ctx}
	end.

delete_resource(Req, Ctx) ->
	#ctx{session = Session, combatant = Combatant, map = Map} = Ctx,
	{ok, Map2} = rpgb_rec_combatant:delete(Combatant, Map),
	{true, Req, Ctx#ctx{map = Map2, combatant = undefined}}.

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

to_json(Req, #ctx{layerid = LayerId} = Ctx) when LayerId =/= undefined ->
	#ctx{map = Map} = Ctx,
	Combatants1 = [rpgb_data:get_by_id(rpgb_rec_combatant, Id) || Id <- Map#rpgb_rec_battlemap.combatant_ids],
	Combatants = [Combatant || {ok, #rpgb_rec_combatant{layer_id = Lid} = Combatant} <- Combatants1, Lid == LayerId],
	Json = [make_json(Req, Ctx, Combatant) || Combatant <- Combatants],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	#ctx{map = Map} = Ctx,
	Combatants1 = [rpgb_data:get_by_id(rpgb_rec_combatant, Id) || Id <- Map#rpgb_rec_battlemap.combatant_ids],
	Combatants = [Combatant || {ok, Combatant} <- Combatants1],
	Json = [make_json(Req, Ctx, Combatant) || Combatant <- Combatants],
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{combatant = Combatant} = Ctx) ->
	Json = make_json(Req, Ctx, Combatant),
	{jsx:to_json(Json), Req, Ctx}.

from_json(Req, #ctx{combatantid = mapcombatants} = Ctx) ->
	#ctx{session = Session, map = Map, mapid = MapId} = Ctx,
	User = rpgb_session:get_user(Session),
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case rpgb_rec_combatant:validate(Term) of
		{ok, {Json, Rec}} ->
			Rec1 = #rpgb_rec_combatant{battlemap_id = MapId, owner_id = User:id(),
				created = os:timestamp(), updated = os:timestamp()},
			{ok, {Rec3, Map1}} = rpgb_rec_combatant:append(Rec1, Map),
			Ctx2 = Ctx#ctx{combatant = Rec3, combatantid = Rec3#rpgb_rec_combatant.id, map = Map1},
			{OutBody, Req3, Ctx3} = to_json(Req1, Ctx2),
			Location = proplists:get_value(url, OutBody),
			Req4 = cowboy_req:set_resp_body(OutBody, Req3),
			{{true, Location}, Req4, Ctx3};
		{error, {invalid, Desc}} ->
			ErrBody = jsx:to_json(Desc),
			Req2 = cowboy_req:set_resp_body(ErrBody, Req1),
			{false, Req2, Ctx}
	end;

from_json(Req, Ctx) ->
	#ctx{map = Map} = Ctx,
	Combatant = Ctx#ctx.combatant#rpgb_rec_combatant{updated = os:timestamp()},
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case rpgb_rec_combatant:validate(Term, Combatant) of
		{ok, {_Json, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			Json = rpgb_rec_combatant:make_json(Rec2),
			Req2 = cowboy_req:set_resp_body(Json, Req),
			{true, Req2, Ctx#ctx{combatant = Rec2}};
		{error, {invalid, Err}} ->
			ErrBody = jsx:to_json(Err),
			Req2 = cowboy_req:set_resp_body(Err, Req1),
			{false, Req2, Ctx}
	end.

generate_etag(Req, #ctx{combatant = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{combatant = Combatant} = Ctx) ->
	Bin = jsx:to_json(Combatant:to_json()),
	Updated = Combatant#rpgb_rec_combatant.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.

%% ===============================
%% internal functions
%% ===============================

make_json(_Req, _Ctx, Combatant) ->
	rpgb_rec_combatant:make_json(Combatant).
