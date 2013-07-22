-module(rpgb_handle_map_websocket).
-behavior(cowboy_websocket_handler).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-else.
-define(debugFmt(_Fmt,_Args), ok).
-define(debugMsg(_Msg), ok).
-endif.

-export([get_routes/0]).
-export([init/3, websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

-record(state, {user, session, map}).

get_routes() ->
	[<<"/maps/:mapid/ws">>].

init(_Protocol, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opt) ->
	BindRes = rpgb:bind({Req, #state{}}, [
		fun get_session/1,
		fun get_user/1,
		fun map_id_is_integer/1,
		fun map_exists/1,
		fun user_is_participant/1,
		fun battle_joined/1
	]),
	case BindRes of
		{ok, {Req1, State}} ->
			{ok, Req1, State};
		{error, Req2} ->
			?warning("Could not setup websocket"),
			{shutdown, Req2}
	end.

websocket_handle({text, Msg}, Req, State) ->
	case jsx:to_term(Msg) of
		[{}] ->
			?debugMsg("empty json means no work done"),
			{ok, Req, State};
		BadJson when not is_list(BadJson) ->
			?debugFmt("bad json decode: ~p", [BadJson]),
			{ok, Req, State};
		Json ->
			Action = proplists:get_value(<<"action">>, Json),
			Type = proplists:get_value(<<"type">>, Json),
			Id = proplists:get_value(<<"id">>, Json),
			Data = proplists:get_value(<<"data">>, Json),
			From = proplists:get_value(<<"reply_with">>, Json),
			?debugFmt("dispatch action ~p for ~p:~p", [Action, Type, Id]),
			dispatch(Req, State, From, Action, Type, Id, Data)
	end;
websocket_handle(_Msg, Req, State) ->
	{ok, Req, State}.

websocket_info({map_event, {update, Record}}, Req, State) ->
	?debugFmt("map update event: ~p", [Record]),
	Frame = make_frame(Record, element(2, Record), <<"put">>, undefined, Record:make_json()),
	State2 = if
		is_record(Record, rpgb_rec_battlemap) ->
			State#state{map = Record};
		true ->
			State
	end,
	{reply, {text, jsx:to_json(Frame)}, Req, State2};

websocket_info({map_event, {new, Record}}, Req, State) ->
	?debugMsg("map new event"),
	Frame = make_frame(Record, element(2, Record), <<"put">>, undefined, Record:make_json()),
	{reply, {text, jsx:to_json(Frame)}, Req, State};

websocket_info({map_event, {delete, rpgb_rec_zone, Id}}, Req, State) ->
	lists:map(fun(BinType) ->
		Frame = make_frame(BinType, Id, <<"delete">>, undefined, undefined),
		BinFrame = jsx:to_json(Frame),
		self() ! {send, BinFrame}
	end, [<<"aura">>, <<"zone">>, <<"scenery">>]),
	{ok, Req, State};

websocket_info({map_event, {delete, Type, Id}}, Req, State) ->
	?debugMsg("map delete event"),
	lager:info("delete event of ~p ~p", [Type, Id]),
	Frame = make_frame(Type, Id, <<"delete">>, undefined, undefined),
	{reply, {text, jsx:to_json(Frame)}, Req, State};

websocket_info({send, Msg}, Req, State) ->
	?debugFmt("some random send: ~p", [Msg]),
	{reply, {text, Msg}, Req, State};
websocket_info(Info, Req, State) ->
	?debugFmt("Some other info: ~p", [Info]),
	lager:warning("unhandled info ~p", [Info]),
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
	rpgb_battle:leave(State#state.map#rpgb_rec_battlemap.id),
	ok.

battle_joined({Req, State}) ->
	case rpgb_battle:join(State#state.map#rpgb_rec_battlemap.id) of
		ok ->
			{ok, {Req, State}};
		Error ->
			?info("Battle could not be joined: ~p", [Error]),
			{ok, Req1} = cowboy_req:reply(500, Req),
			{error, {Req1, State}}
	end.

get_session({Req, _State}) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	State2 = #state{session = Session},
	{ok, {Req1, State2}}.

get_user({Req, State}) ->
	case rpgb_session:get_user(State#state.session) of
		undefined ->
			?info("No user associated with session"),
			{ok, Req1} = cowboy_req:reply(401, Req),
			{error, Req1};
		User ->
			{ok, {Req, State#state{user = User}}}
	end.

map_id_is_integer({Req, State}) ->
	{MapId, Req1} = cowboy_req:binding(mapid, Req),
	try list_to_integer(binary_to_list(MapId)) of
		N ->
			State2 = State#state{map = N},
			{ok, {Req, State2}}
	catch
		error:badarg ->
			?info("map id provided is not an integer"),
			{ok, Req3} = cowboy_req:reply(404, Req1),
			{error, Req3}
	end.

map_exists({Req, State}) ->
	case rpgb_data:get_by_id(rpgb_rec_battlemap, State#state.map) of
		{error, notfound} ->
			?info("Map doesn't exist"),
			{ok, Req1} = cowboy_req:reply(404, Req),
			{error, Req1};
		{ok, Map} ->
			{ok, {Req, State#state{map = Map}}}
	end.

user_is_participant({Req, State}) ->
	#state{user = User, map = Map} = State,
	?info("Checking if user ~p is participant of map ~p", [User#rpgb_rec_user.id, Map]),
	case rpgb_rec_battlemap:is_user_participant(User, Map) of
		true ->
			{ok, {Req, State}};
		false ->
			{ok, Req1} = cowboy_req:reply(403, Req),
			{error, Req1}
	end.

user_is_owner(State) ->
	#state{user = User, map = Map} = State,
	rpgb_rec_battlemap:is_user_owner(User, Map).

make_reply(From, Accepted, Data) ->
	Json = make_frame(<<"reply">>, From, <<"reply">>, Accepted, Data),
	jsx:to_json(Json).

make_frame(Type, Id, Action, Accepted, Data) ->
	Json1 = maybe_add_type(Type, []),
	Json2 = maybe_add_id(Id, Json1),
	Json3 = maybe_add_accepted(Accepted, Json2),
	Json4 = maybe_add_data(Data, Json3),
	[{<<"action">>, Action} | Json4].

maybe_add_type(Type, Json) when is_record(Type, rpgb_rec_zone) ->
	Type2 = list_to_binary(atom_to_list(Type#rpgb_rec_zone.type)),
	maybe_add_type(Type2, Json);
maybe_add_type(Tuple, Json) when is_tuple(Tuple) ->
	Type2 = element(1, Tuple),
	maybe_add_type(Type2, Json);
maybe_add_type(rpgb_rec_battlemap, Json) ->
	[{<<"type">>, <<"map">>} | Json];
maybe_add_type(Type, Json) when is_binary(Type) ->
	[{<<"type">>, Type} | Json];
maybe_add_type(rpgb_rec_layer, Json) ->
	[{<<"type">>, <<"layer">>} | Json];
maybe_add_type(rpgb_rec_combatant, Json) ->
	[{<<"type">>, <<"combatant">>} | Json];
maybe_add_type(Type, Json) when is_binary(Type) ->
	[{<<"type">>, Type} | Json];
maybe_add_type(Huh, Json) ->
	?debugFmt("I don't understand the type ~p", [Huh]),
	lager:warning("I don't understand the type ~p", [Huh]),
	Json.

maybe_add_id(undefined, Json) ->
	Json;
maybe_add_id(Id, Json) ->
	[{<<"type_id">>, Id} | Json].

maybe_add_accepted(undefined, Json) ->
	Json;
maybe_add_accepted(Bool, Json) ->
	[{<<"accepted">>, Bool} | Json].

maybe_add_data(undefined, Json) ->
	Json;
maybe_add_data(Data, Json) ->
	[{<<"data">>, Data} | Json].

dispatch(Req, State, undefined, _Action, _Type, _Id, _Data) ->
	?debugMsg("no reply id given"),
	{ok, Req, State};

dispatch(Req, State, From, <<"put">>, Type, Id, undefined) ->
	?debugMsg("no data put"),
	Reply = make_reply(From, false, <<"no data">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, #state{map = #rpgb_rec_battlemap{owner_id = Owner}, user = #rpgb_rec_user{id = Owner}} = State, From, <<"put">>, <<"map">>, Id, Json) ->
	case rpgb_rec_battlemap:update_from_json(Json, State#state.map) of
		{ok, Map2} ->
			{ok, Map3} = rpgb_data:save(Map2),
			ReplyFrame = make_reply(From, true, rpgb_rec_battlemap:make_json(Map3)),
			?debugFmt("All the world is good updating map: ~p", [ReplyFrame]),
			{reply, {text, ReplyFrame}, Req, State#state{map = Map3}};
		{error, {Wut, English}} ->
			?debugFmt("makeing a reply due to bad update: ~p:~p", [Wut, English]),
			ReplyFrame = make_reply(From, false, English),
			{reply, {text, ReplyFrame}, Req, State}
	end;

dispatch(Req, State, From, <<"put">>, <<"map">>, Id, Json) ->
	?debugMsg("not owner"),
	Reply = make_reply(From, false, <<"not owner">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, <<"map">>, _Id, _Json) ->
	?debugMsg("basic get"),
	case rpgb_data:get_by_id(rpgb_rec_battlemap, State#state.map#rpgb_rec_battlemap.id) of
		{ok, Map} ->
			Reply = make_reply(From, true, rpgb_rec_battlemap:make_json(Map)),
			{reply, {text, Reply}, Req, State#state{map = Map}};
		{error, notfound} ->
			Reply = make_reply(From, false, <<"map not found">>),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"delete">>, <<"map">>, _Id, _Json) ->
	?debugMsg("can't do a delete"),
	Reply = make_reply(From, false, undefined),
	{reply, {text, Reply}, Req, State};

dispatch(Req, #state{map = #rpgb_rec_battlemap{owner_id = Owner, id = Id} = Map, user = #rpgb_rec_user{id = Owner}} = State, From, <<"post">>, <<"map">>, Id, Json) ->
	Action = proplists:get_value(<<"action">>, Json),
	Args = proplists:get_value(<<"args">>, Json),
	Reply = case {Action, Args} of
		{<<"invite">>, [Partier]} when is_binary(Partier) ->
			case rpgb_rec_user:get_maybe_created(Partier) of
				{ok, PartierRec} ->
					case rpgb_rec_battlemap:is_user_participant(PartierRec, State#state.map) of
						true ->
							make_reply(From, false, <<"already participant">>);
						false ->
							Plist = [PartierRec#rpgb_rec_user.id | State#state.map#rpgb_rec_battlemap.participant_ids],
							{ok, Map2} = rpgb_data:save(Map#rpgb_rec_battlemap{participant_ids = Plist}),
							make_reply(From, true, Partier)
					end;
				Else ->
					lager:info("coudn't invite partier ~p", [Else]),
					make_reply(From, false, iolist_to_binary(io_lib:format("could finish invite: ~p", [Else])))
			end;
		_ ->
			lager:info("not doing action ~p with args ~p", [Action, Args]),
			make_reply(From, false, <<"invalid posted action or args">>)
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, _Action, <<"map">>, _Id, _Json) ->
	Reply = make_reply(From, false, <<"invalid method">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, <<"layer">>, undefined, _Json) ->
	Reply = make_reply(From, false, <<"must specify layer">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, <<"layer">>, Id, _Json) ->
	UserIsOwner = user_is_owner(State),
	MapId = State#state.map#rpgb_rec_battlemap.id,
	Reply = if
		UserIsOwner ->
			case rpgb_data:get_by_id(rpgb_rec_layer, Id) of
				{error, notfound} ->
					make_reply(From, false, <<"not_found">>);
				{ok, #rpgb_rec_layer{battlemap_id = MapId} = Layer} ->
					case rpgb_rec_layer:delete(Layer, State#state.map) of
						{ok, Map2} ->
							lager:info("deleted layer ~p", [Id]),
							make_reply(From, true, undefined);
						{error, last_layer} ->
							make_reply(From, false, <<"cannot delete last layer">>);
						{error, Wut} ->
							make_reply(From, false, iolist_to_binary(io_lib:format("unknown error: ~p", [Wut])))
					end;
				{ok, _Layer} ->
					make_reply(From, false, <<"layer not part of map">>)
			end;
		true ->
			make_reply(From, false, <<"only owner can delete layers">>)
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, <<"layer">>, undefined, _Json) ->
	DataReses = lists:map(fun(Id) ->
		Res = rpgb_data:get_by_id(rpgb_rec_layer, Id),
		element(2, Res)
	end, State#state.map#rpgb_rec_battlemap.layer_ids),
	Layers = lists:filter(fun(MaybeLayer) ->
		is_record(MaybeLayer, rpgb_rec_layer)
	end, DataReses),
	Json = lists:map(fun(Layer) ->
		rpgb_rec_layer:make_json(Layer)
	end, Layers),
	Reply = make_reply(From, true, Json),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, <<"layer">>, Id, _Json) ->
	Reply = case rpgb_data:get_by_id(rpgb_rec_layer, Id) of
		{ok, #rpgb_rec_layer{battlemap_id = MapId} = Layer} when MapId =:= State#state.map#rpgb_rec_battlemap.id ->
				Json = rpgb_rec_layer:make_json(Layer),
				make_reply(From, true, Json);
		_ ->
			make_reply(From, false, <<"layer not found">>)
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"put">>, <<"layer">>, undefined, _Json) ->
	Reply = make_reply(From, false, <<"cannot put to layers list">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"put">>, <<"layer">>, Id, Json) ->
	Reply = case user_is_owner(State) of
		false ->
			make_reply(From, false, <<"only owner may edit map layers">>);
		true ->
			case rpgb_rec_layer:update_from_json(Json, Id) of
				{ok, Rec} ->
					{ok, Rec2} = rpgb_data:save(Rec),
					OutJson = rpgb_rec_layer:make_json(Rec2),
					make_reply(From, true, OutJson);
				{error, {_Atom, Msg}} ->
					make_reply(From, false, Msg)
			end
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"post">>, <<"layer">>, undefined, Json) ->
	Reply = case user_is_owner(State) of
		false ->
			make_reply(From, false, <<"only owner may edit map layers">>);
		true ->
			InitLayer = #rpgb_rec_layer{
				battlemap_id = State#state.map#rpgb_rec_battlemap.id,
				created = os:timestamp(),
				updated = os:timestamp()
			},
			case rpgb_rec_layer:update_from_json(Json, InitLayer) of
				{ok, Rec} ->
					{ok, Rec2} = rpgb_data:save(Rec),
					Map = State#state.map,
					MapLayers = Map#rpgb_rec_battlemap.layer_ids ++ [Rec2#rpgb_rec_layer.id],
					rpgb_data:save(Map#rpgb_rec_battlemap{layer_ids = MapLayers}),
					make_reply(From, true, rpgb_rec_layer:make_json(Rec2));
				{error, {_Atom, Msg}} ->
					make_reply(From, false, Msg)
			end
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"post">>, <<"layer">>, _Layer, _Json) ->
	Reply = make_reply(From, false, <<"invalid action">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, <<"combatant">>, undefined, _Json) ->
	Reply = make_reply(From, false, <<"must specify combatant">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, <<"combatant">>, Id, _Json) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, Id) of
		{ok, #rpgb_rec_combatant{battlemap_id = MapId} = Combatant} when MapId =:= State#state.map#rpgb_rec_battlemap.id ->
			case rpgb_rec_combatant:delete(Combatant, State#state.map) of
				{ok, Map2} ->
					Reply = make_reply(From, true, undefined),
					{reply, {text, Reply}, Req, State#state{map = Map2}};
				{error, Wut} ->
					Reply = make_reply(From, false, iolist_to_binary(io_lib:format("unknown error: ~p", [Wut]))), 
					{reply, {text, Reply}, Req, State}
			end;
		_ ->
			Reply = make_reply(From, false, <<"invalid combatant">>),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"get">>, <<"combatant">>, undefined, _Json) ->
	Combatants = lists:map(fun(Id) ->
		{ok, Combatant} = rpgb_data:get_by_id(rpgb_rec_combatant, Id),
		Combatant
	end, State#state.map#rpgb_rec_battlemap.combatant_ids),
	Json = lists:map(fun(C) ->
		rpgb_rec_combatant:make_json(C)
	end, Combatants),
	Reply = make_reply(From, true, Json),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, <<"combatant">>, Id, _Json) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, Id) of
		{ok, #rpgb_rec_combatant{battlemap_id = MapId} = Combatant} when MapId =:= State#state.map#rpgb_rec_battlemap.id ->
			Json = rpgb_rec_combatant:make_json(Combatant),
			Reply = make_reply(From, true, Json),
			{reply, {text, Reply}, Req, State};
		_ ->
			Reply = make_reply(From, false, <<"invalid combatant">>),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"put">>, <<"combatant">>, undefined, _Json) ->
	Reply = make_reply(From, false, <<"cannot put to combatants list">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"put">>, <<"combatant">>, Id, Json) ->
	case rpgb_data:get_by_id(rpgb_rec_combatant, Id) of
		{ok, #rpgb_rec_combatant{battlemap_id = MapId} = Combatant} when MapId =:= State#state.map#rpgb_rec_battlemap.id ->
			case rpgb_rec_combatant:validate(Json, Combatant) of
				{ok, {Json2, Rec}} ->
					{ok, Rec2} = rpgb_data:save(Rec),
					Reply = make_reply(From, true, rpgb_rec_combatant:make_json(Rec2)),
					{reply, {text, Reply}, Req, State};
				{error, {invalid, Txt}} ->
					Reply = make_reply(From, false, jsx:to_json(Txt)),
					{reply, {text, Reply}, Req, State}
			end;
		_ ->
			Reply = make_reply(From, false, jsx:to_json(<<"invalid combatant">>)),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"post">>, <<"combatant">>, undefined, Json) ->
	BaseRec = #rpgb_rec_combatant{battlemap_id = State#state.map#rpgb_rec_battlemap.id},
	case rpgb_rec_combatant:validate(Json, BaseRec) of
		{ok, {_Json2, Rec}} ->
			Rec2 = Rec#rpgb_rec_combatant{
				battlemap_id = State#state.map#rpgb_rec_battlemap.id,
				owner_id = State#state.user#rpgb_rec_user.id,
				created = os:timestamp(),
				updated = os:timestamp()
			},
			{ok, {Rec3, Map2}} = rpgb_rec_combatant:append(Rec2, State#state.map),
			Reply = make_reply(From, true, rpgb_rec_combatant:make_json(Rec3)),
			{reply, {text, Reply}, Req, State};
		{error, {invalid, Txt}} ->
			Reply = make_reply(From, false, Txt),
			{reply, {text, Reply}, Req, State}
	end;

dispatch(Req, State, From, <<"post">>, <<"combatant">>, _Id, _Json) ->
	Reply = make_reply(From, false, <<"invalid action">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, Action, Type, Id, Json) when Type =:= <<"aura">>; Type =:= <<"zone">>; Type =:= <<"scenery">> ->
	Mode = list_to_atom(binary_to_list(Type)),
	dispatch(Req, State, From, Action, {rpgb_rec_zone, Mode}, Id, Json);

dispatch(Req, State, From, <<"post">>, {rpgb_rec_zone, Mode}, undefined, Json) ->
	MapId = State#state.map#rpgb_rec_battlemap.id,
	Reply = case lists:keytake(<<"layer_id">>, 1, Json) of
		{value, {<<"layer_id">>, LayerId}, Json2} ->
			case rpgb_data:get_by_id(rpgb_rec_layer, LayerId) of
				{ok, #rpgb_rec_layer{battlemap_id = MapId} = Layer} ->
					BaseRec = #rpgb_rec_zone{layer_id = LayerId, name = <<>>, type = Mode, created = os:timestamp(), updated = os:timestamp()},
					case rpgb_rec_zone:validate(Json2, BaseRec) of
						{ok, {_, Rec}} ->
							{ok, Rec2} = rpgb_data:save(Rec),
							{ok, Layer2} = rpgb_rec_zone:append(Layer, Rec2),
							OutJson = rpgb_rec_zone:make_json(Rec2),
							make_reply(From, true, OutJson);
						{error, {invalid, Txt}} ->
							make_reply(From, false, Txt);
						{error, {conflict, Txt}} ->
							make_reply(From, false, Txt)
					end;
				_ ->
					make_reply(From, false, <<"invalid layer id given">>)
			end;
		_ ->
			make_reply(From, false, <<"invalid layer id given">>)
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"post">>, {rpgb_rec_zone, _Mode}, _Id, _Json) ->
	Reply = make_reply(From, false, <<"invalid action">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"put">>, {rpgb_rec_zone, _Mode}, undefined, _Json) ->
	Reply = make_reply(From, false, <<"invalid action">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"put">>, {rpgb_rec_zone, Mode}, Id, Json) ->
	Reply = case rpgb_data:search(rpgb_rec_zone, [{id, Id}, {type, Mode}]) of
		{ok, []} ->
			make_reply(From, false, <<"not found">>);
		{ok, [Zone | _]} ->
			case rpgb_rec_zone:validate(Json, Zone) of
				{ok, {_, Rec}} ->
					{ok, Rec2} = rpgb_data:save(Rec),
					make_reply(From, true, rpgb_rec_zone:make_json(Rec2));
				{error, {_ErrAtom, Txt}} ->
					make_reply(From, false, Txt)
			end
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, {rpgb_rec_zone, _Mode}, undefined, _Json) ->
	Reply = make_reply(From, false, <<"invalid action">>),
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"delete">>, {rpgb_rec_zone, Mode}, Id, Json) ->
	Reply = case rpgb_data:search(rpgb_rec_zone, [{id, Id}, {type, Mode}]) of
		{ok, []} ->
			make_reply(From, false, <<"not found">>);
		{ok, [Zone | _]} ->
			MapId = State#state.map#rpgb_rec_battlemap.id,
			case rpgb_data:get_by_id(rpgb_rec_layer, Zone#rpgb_rec_zone.layer_id) of
				{ok, #rpgb_rec_layer{battlemap_id = MapId} = Layer} ->
					rpgb_rec_zone:delete(Zone, Layer),
					make_reply(From, true, <<>>);
				_ ->
					make_reply(From, false, <<"not found">>)
			end
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, {rpgb_rec_zone, Mode}, undefined, Json) ->
	Json2 = case Json of undefined -> []; _ -> Json end,
	Reply = case proplists:get_value(<<"layer_id">>, Json2) of
		undefined ->
			LayerIds = State#state.map#rpgb_rec_battlemap.layer_ids,
			ZoneIdsLists = lists:map(fun(Id) ->
				{ok, Layer} = rpgb_data:get_by_id(rpgb_rec_layer, Id),
				case Mode of
					aura -> Layer#rpgb_rec_layer.aura_ids;
					zone -> Layer#rpgb_rec_layer.zone_ids;
					scenery -> Layer#rpgb_rec_layer.scenery_ids
				end
			end, LayerIds),
			ZoneIds = lists:flatten(ZoneIdsLists),
			OutJson = lists:map(fun(Id) ->
				{ok, Zone} = rpgb_data:get_by_id(rpgb_rec_zone, Id),
				rpgb_rec_zone:make_json(Zone)
			end, ZoneIds),
			make_reply(From, true, OutJson);
		LayerId ->
			MapId = State#state.map#rpgb_rec_battlemap.id,
			case rpgb_data:get_by_id(rpgb_rec_layer, LayerId) of
				{ok, #rpgb_rec_layer{battlemap_id = MapId} = Layer} ->
					IdList = case Mode of
						aura -> Layer#rpgb_rec_layer.aura_ids;
						zone -> Layer#rpgb_rec_layer.zone_ids;
						scenery -> Layer#rpgb_rec_layer.scenery_ids
					end,
					OutJson = lists:map(fun(Id) ->
						{ok, Z} = rpgb_data:get_by_id(rpgb_rec_zone, Id),
						rpgb_rec_zone:make_json(Z)
					end, IdList),
					make_reply(From, true, OutJson);
				_ ->
					make_reply(From, false, <<"not found">>)
			end
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, <<"get">>, {rpgb_rec_zone, Mode}, Id, _Json) ->
	Reply = case rpgb_data:get_by_id(rpgb_rec_zone, Id) of
		{ok, #rpgb_rec_zone{type = Mode} = Zone} ->
			MapId = State#state.map#rpgb_rec_battlemap.id,
			case rpgb_data:get_by_id(rpgb_rec_layer, Zone#rpgb_rec_zone.layer_id) of
				{ok, #rpgb_rec_layer{battlemap_id = MapId}} ->
					make_reply(From, true, rpgb_rec_zone:make_json(Zone));
				_ ->
					make_reply(From, false, <<"not found">>)
			end;
		_ ->
			make_reply(From, false, <<"not found">>)
	end,
	{reply, {text, Reply}, Req, State};

dispatch(Req, State, From, Action, Type, Id, Data) ->
	?debugFmt("no reply, just not gonna do anything~n"
		"    State: ~p~n"
		"    From: ~p~n"
		"    Action: ~p~n"
		"    Type: ~p~n"
		"    Id: ~p~n"
		"    Data: ~p", [State, From, Action, Type, Id, Data]),
	Reply = make_reply(From, false, <<"nyi">>),
	{reply, {text, Reply}, Req, State}.
