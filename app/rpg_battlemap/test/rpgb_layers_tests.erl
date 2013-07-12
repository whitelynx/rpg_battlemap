-module(rpgb_layers_tests).

-include("prop_tests.hrl").

simple_args(http, [Map, Who, Action, undefined, _Json, Name]) ->
	MapId = case Map of
		undefined -> undefined;
		_ -> Map#test_map.id
	end,
	[MapId, Who, Action, undefined, Name];
simple_args(http, [Map, Who, Action, #test_layer{id = Id}, _Json, Name]) ->
	MapId = case Map of
		undefined -> undefined;
		_ -> Map#test_map.id
	end,
	[MapId, Who, Action, Id, Name];
simple_args(websocket, [{_Socket, Who, MapId}, Action, undefined, _Json, Name]) ->
	[MapId, Who, Action, undefined, Name];
simple_args(websocket, [{_Socket, Who, MapId}, Action, #test_layer{id = Id}, _Json, Name]) ->
	[MapId, Who, Action, Id, Name];
simple_args(http_reorder, [Who, #test_map{id = Id}, _Order, _Layers]) ->
	[Who, Id];
simple_args(http_reorder, [Who, undefined, _Order, _Layers]) ->
	[Who, undefined];
simple_args(websocket_reorder, [{_Socket, Who, Id}, _Order, _Layers]) ->
	[Who, Id].

%% === commands ==========

command(#state{maps = []}) ->
	[];
command(State) -> [
	{call, ?MODULE, http, [rpgb_requests_tests:g_maybe_exists(State#state.maps), rpgb_requests_tests:g_who(), rpgb_requests_tests:g_action(), rpgb_requests_tests:g_maybe_exists(State#state.layers), rpgb_prop:g_layerjson(), rpgb_prop:g_maybe_name()]},
	{call, ?MODULE, websocket, [rpgb_requests_tests:g_maybe_exists(State#state.ws), rpgb_requests_tests:g_action(), rpgb_requests_tests:g_maybe_exists(State#state.layers), rpgb_prop:g_layerjson(), rpgb_prop:g_maybe_name()]}
	%,{call, ?MODULE, http_reorder, [rpgb_requests_tests:g_who(), rpgb_requests_tests:g_maybe_exists(State#state.maps), g_shuffle_order(State#state.layers), State#state.layers]}
	%,{call, ?MODULE, websocket_reorder, [rpgb_requests_tests:g_maybe_exists(State#state.ws), g_shuffle_order(State#state.layers), State#state.layers]}
	].

g_shuffle_order(List) ->
	[random:uniform() || _ <- List].

%% === commands ==========

http(Map, Who, Action, Layer, Json, Name) ->
	Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
	Murl = case {Map, Layer} of
		{undefined, undefined} ->
			<<>>;
		{undefined, _} ->
			Layer#test_layer.map_url;
		{_, _} ->
			Map#test_map.url
	end,
	rpgb_requests_tests:send_request(layers, http, Who, Action, Layer, Json2, [{map, Murl}]).

websocket({_Ws, Who, _MapId} = Socket, Action, Layer, Json, Name) ->
	Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
	rpgb_requests_tests:send_request(layers, Socket, Who, Action, Layer, Json2, []).

http_reorder(Who, #test_map{id = MapId} = Map, Order, Layers) ->
	Ids = [Layer#test_layer.id || #test_layer{map_id = Mid} = Layer <- Layers, Mid =:= MapId],
	Zipped = zip(Order, Ids),
	Shuffled = [Id || {_, Id} <- lists:sort(Zipped)],
	Json = [{<<"layer_order">>, Shuffled}],
	rpgb_requests_tests:send_request(maps, http, Who, put, Map, Json, []).

websocket_reorder({_Ws, Who, MapId} = Socket, Order, Layers) ->
	Ids = [Layer#test_layer.id || #test_layer{map_id = Mid} = Layer <- Layers, Mid =:= MapId],
	Zipped = zip(Order, Ids),
	Shuffled = [Id || {_, Id} <- lists:sort(Zipped)],
	Json = [{<<"layer_order">>, Shuffled}],
	rpgb_requests_tests:send_request(maps, Socket, Who, put, MapId, Json, []).

%% === precondtion =======

precondition(_State, {call, ?MODULE, http, [undefined, _Who, _Action, undefined, _Json, _Name]}) ->
	false;

precondition(_State, {call, ?MODULE, http_reorder, [_Who, undefined, _Order]}) ->
	false;

precondition(_State, {call, ?MODULE, websocket, [undefined | _]}) ->
	false;

precondition(_State, {call, ?MODULE, websocket_reorder, [undefined | _]}) ->
	false;

precondition(_State, _Call) ->
	true.

%% === next_state ========

next_state(State, {ok, "201", _Heads, _Body} = Res, {call, ?MODULE, http, [Map, _Who, post, undefined, _Json, _Name]}) ->
	Layer = extract_layer(undefined, Res),
	Layer2 = Layer#test_layer{
		map_id = Map#test_map.id,
		map_url = Map#test_map.url
	},
	Layers = Map#test_map.layers ++ [Layer#test_layer.id],
	Map2 = Map#test_map{layers = Layers},
	Maps = lists:keystore(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
	LayersList = lists:keystore(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
	State#state{maps = Maps, layers = LayersList};

next_state(State, _Res, {call, ?MODULE, http, [_Map, _Who, delete, undefined, _Json, _Name]}) ->
	State;

next_state(State, {ok, "204", _Heads, _Body}, {call, ?MODULE, http, [_Map, _Who, delete, Layer, _Json, _Name]}) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	MapLayers = lists:delete(Layer#test_layer.id, Map#test_map.layers),
	Map2 = Map#test_map{layers = MapLayers},
	Layers = lists:keydelete(Layer#test_layer.id, #test_layer.id, State#state.layers),
	Maps = lists:keystore(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
	State#state{maps = Maps, layers = Layers};

next_state(State, {ok, "200", _Head, _Body} = Res, {call, ?MODULE, http, [_Map, _Who, put, Layer, _Json, _Name]}) ->
	Layer2 = extract_layer(Layer, Res),
	Layers = lists:keystore(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
	State#state{layers = Layers};

next_state(State, _Res, {call, ?MODULE, websocket, [{Socket, Who, MapId}, delete, #test_layer{map_id = MapId} = Layer | _]}) ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			Layers = lists:keydelete(Layer#test_layer.id, #test_layer.id, State#state.layers),
			State#state{layers = Layers};
		#test_map{owner = Who} ->
			Layers = lists:keydelete(Layer#test_layer.id, #test_layer.id, State#state.layers),
			State#state{layers = Layers};
		_ ->
			State
	end;

next_state(State, Res, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, #test_layer{map_id = MapId} = Layer, _Json, Name]}) when is_binary(Name), Name =/= <<>>; Name =:= undefined ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			State;
		#test_map{owner = Who} ->
			Layer2 = extract_layer(Layer, Res),
			Layers = lists:keystore(Layer2#test_layer.id, #test_layer.id, State#state.layers, Layer2),
			State#state{layers = Layers};
		_ ->
			State
	end;

next_state(State, Res, {call, ?MODULE, websocket, [{Socket, Who, MapId}, post, undefined, _Json, Name]}) when is_binary(Name), Name =/= <<>> ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			State;
		#test_map{owner = Who} = Map ->
			Layer = extract_layer(undefined, Res),
			Layer2 = Layer#test_layer{map_id = MapId, map_url = Map#test_map.url},
			Layers = lists:keystore(Layer#test_layer.id, #test_layer.id, State#state.layers, Layer2),
			MapLayers = Map#test_map.layers ++ [Layer2#test_layer.id],
			Map2 = Map#test_map{layers = MapLayers},
			Maps = lists:keystore(Map2#test_map.id, #test_map.id, State#state.maps, Map2),
			State#state{maps = Maps, layers = Layers};
		_ ->
			State
	end;

next_state(State, _Result, {call, ?MODULE, _Func, _Args}) ->
	State.

%% === postcondition =====

postcondition(_State, {call, ?MODULE, http, [_Map, _Who, delete, undefined, _Json, _Name]}, {ok, "405", _Heads, _Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [_Map, _Who, put, undefined, _Josn, _Name]}, {ok, "405", _Heads, _Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [_Map, notauthed | _]}, {ok, "401", _Heads, _Body}) ->
	true;

postcondition(_State, {call, ?MODULE, http, [_Map, notpartier | _]}, {ok, "403", _Heads, _Body}) ->
	true;

postcondition(State, {call, ?MODULE, http, [Map, _Who, get, undefined, _Json, _Name]}, {ok, "200", _Heads, Body}) ->
	BodyJsonGot = jsx:to_term(list_to_binary(Body)),
	StateMap = lists:keyfind(Map#test_map.id, #test_map.id, State#state.maps),
	Layers = [lists:keyfind(Lid, #test_layer.id, State#state.layers) || Lid <- StateMap#test_map.layers],
	if
		length(BodyJsonGot) == length(Layers) ->
			lists:all(fun({Layer, Json}) ->
				rpgb_requests_tests:assert_json(Layer#test_layer.properties, Json)
			end, zip(Layers, BodyJsonGot));
		true ->
			?debugFmt("different number of layers", []),
			false
	end;

postcondition(State, {call, ?MODULE, http, [_Map, _Who, get, Layer, _Json, _Name]}, {ok, "200", _Heads, Body}) ->
	Got = jsx:to_term(list_to_binary(Body)),
	StateLayer = lists:keyfind(Layer#test_layer.id, #test_layer.id, State#state.layers),
	rpgb_requests_tests:assert_json(StateLayer#test_layer.properties, Got);

postcondition(State, {call, ?MODULE, http, [_Map, Owner, delete, Layer, _Json, _Name]}, {ok, "422", _Heads, Body}) ->
	BodyMatch = Body =:= "you cannot delete the last layer of a map",
	MapId = Layer#test_layer.map_id,
	Map = lists:keyfind(MapId, #test_map.id, State#state.maps),
	case Map of
		#test_map{owner= Owner, layers = [_L1]} ->
			BodyMatch;
		_ ->
			?debugFmt("Maybe should have been able to delete layer: ~p, ~p", [Map, Layer]),
			false
	end;

postcondition(_State, {call, ?MODULE, http, [#test_map{owner = Owner}, NotOwner, NotGet | _]}, {ok, "403", _Heads, _Body}) when Owner =/= NotOwner, NotGet =/= get ->
	true;

postcondition(_State, {call, ?MODULE, http, [#test_map{owner = Owner}, Owner, post, undefined, Json, Name]}, {ok, "422", _Heads, "\"name cannot be blank\""}) ->
	case proplists:get_value(<<"name">>, Json) of
		_ when Name =:= <<>>; name =:= null ->
			true;
		undefined when Name =:= undefined ->
			true;
		_ ->
			?debugFmt("A valid name was set", []),
			false
	end;

postcondition(State, {call, ?MODULE, http, [#test_map{owner = Owner} = Map, Owner, post, undefined, Json, Name]}, {ok, "201", Heads, Body}) ->
	Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
	Got = jsx:to_term(list_to_binary(Body)),
	ValidHead = undefined =/= proplists:get_value("location", Heads),
	Sockets = [Tuple || {_, _, MapId} = Tuple <- State#state.ws, MapId =:= Map#test_map.id],
	case proplists:get_value("location", Heads) of
		undefined ->
			false;
		_ ->
			case rpgb_requests_tests:assert_json(Json2, Got) of
				true ->
					DefFrams = lists:all(fun({Socket, Who, _}) ->
						%?debugFmt("Asserting a socket for ~p", [Who]),
						case rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, layer, proplists:get_value(<<"id">>, Got), Got) of
							true ->
								rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, map, Map#test_map.id, []);
							_ ->
								?debugMsg("layer frame fail")
						end
					end, Sockets);
				_ ->
					false
			end
	end;



postcondition(_State, {call, ?MODULE, http, [_Map, _Who, post, _Layer, _Json, _Name]}, {ok, "405", _Heads, _Body}) ->
	true;

postcondition(State, {call, ?MODULE, http, [_Map, Who, delete, Layer, _Json, _Name]}, {ok, "403", _Heads, _Body}) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	case Map#test_map.owner of
		Who ->
			?debugFmt("owner should only end up getting a 422 if they can't delete", []),
			false;
		_ ->
			true
	end;

postcondition(_State, {call, ?MODULE, http, [_Map, _Who, put, undefined, _Json, _Name]}, {ok, "405", _Heads, _Json}) ->
	true;

postcondition(State, {call, ?MODULE, http, [_Map, Who, put, Layer, Json, Name]}, {ok, "200", _Heads, Body}) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	case Map#test_map.owner of
		Who ->
			Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
			Got = jsx:to_term(list_to_binary(Body)),
			JsonOkay = rpgb_requests_tests:assert_json(Json2, Got),
			Sockets = [Tuple || {_, _, MapId} = Tuple <- State#state.ws, MapId =:= Layer#test_layer.map_id],
			JsonOkay andalso lists:all(fun({Socket, SomeWho, _Mapid}) ->
				rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, layer, Layer#test_layer.id, [])
			end, Sockets);
		_ ->
			?debugFmt("Should have gotten a 403", []),
			false
	end;

postcondition(State, {call, ?MODULE, http, [_Map, Who, put, Layer, _Json, _Name]}, {ok, "403", _Heads, _Body}) ->
	case get_map(Layer, State) of
		#test_map{owner = Who} ->
			?debugFmt("Should have been able to edit layer", []),
			false;
		_ ->
			true
	end;

postcondition(_State, {call, ?MODULE, http, [_Map, _Who, put, _Layer, _Json, Name]}, {ok, "422", _Heads, Body}) when Name == <<>>; Name == null ->
	Body =:= "\"name cannot be blank.\"";

postcondition(_State, {call, ?MODULE, http, [_Map, _Who, post, _Layer, _Json, Name]}, {ok, "422", _Heads, Body}) when Name == <<>>; Name == null ->
	Body =:= "\"name cannot be blank.\"";

postcondition(State, {call, ?MODULE, http, [_Map, Who, delete, Layer, _Json, _Name]}, {ok, "204", _Heads, _Body}) ->
	Map = lists:keyfind(Layer#test_layer.map_id, #test_map.id, State#state.maps),
	case Map of
		false ->
			?debugFmt("Map not found for layer ~p", [Layer]),
			false;
		#test_map{owner = Who} ->
			Sockets = [Socket || {Socket, _Who, MapId} <- State#state.ws, MapId =:= Layer#test_layer.map_id],
			lists:all(fun(Socket) ->
				rpgb_requests_tests:assert_ws_frame(Socket, put, undefined, map, Layer#test_layer.map_id, [])
				andalso
				rpgb_requests_tests:assert_ws_frame(Socket, delete, undefined, layer, Layer#test_layer.map_id, [])
			end, Sockets);
		_ ->
			?debugFmt("Who ~p should not have been able to delete layer ~p for map ~p", [Who, Layer, Map]),
			false
	end;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, delete, #test_layer{map_id = MapId} = Layer | _]}, {ResId, ReplyFrame}) ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, []);
		#test_map{owner = Who} ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ResId, []);
		_ ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"only the map owner can update layers">>)
	end;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, put, #test_layer{map_id = MapId} = Lyaer, Json, Name]}, {ResId, ReplyFrame}) when is_binary(Name), Name =/= <<>>; Name =:= undefined ->
	Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, []);
		#test_map{owner = Who} ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ResId, Json2);
		_ ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"only the map owner can update layers">>)
	end;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, post, undefined, Json, Name]}, {ResId, ReplyFrame}) ->
	Json2 = rpgb_requests_tests:maybe_set_name(Json, Name),
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, []);
		#test_map{owner = Who} when is_binary(Name), Name =/= <<>> ->
			ReplyTrue = rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, true, reply, ResId, Json2),
			SocketsToCheck = [Tuple || {_Socket, _Who, Mid} = Tuple <- State#state.ws, Mid =:= MapId],
			TestFun = fun({Sock, _, _}) ->
					{ok, {text, Binary}} = ReplyFrame,
					DerJson = jsx:to_term(Binary),
					ExpectId = proplists:get_value(<<"id">>, DerJson),
					Frame = gen_websocket:recv(Sock, 1000),
					GotLayer = rpgb_requests_tests:assert_ws_frame(Frame, put, undefined, layer, ExpectId, Json2),
					MapFrame = gen_websocket:recv(Socket, 1000),
					GotMap = rpgb_requests_tests:assert_ws_frame(MapFrame, put, undefined, map, MapId, []),
					GotMap andalso GotLayer
				end,
				ReplyTrue andalso lists:all(TestFun, SocketsToCheck);
		#test_map{owner = Who} ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"invalid name">>);
		_ ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"only owner can update layers">>)
	end;

postcondition(State, {call, ?MODULE, websocket, [{Socket, Who, MapId}, post, _Layer, _Json, _Name]}, {ResId, ReplyFrame}) ->
	case lists:keyfind(MapId, #test_map.id, State#state.maps) of
		false ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, []);
		#test_map{owner = Who} ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"invalid action">>);
		_ ->
			rpgb_requests_tests:assert_ws_frame(ReplyFrame, reply, false, reply, ResId, <<"invalid action">>)
	end;

postcondition(State, Call, Res) ->
	?debugFmt("Fall through postcondition:~n"
		"    State: ~p~n"
		"    Call: ~p~n"
		"    Res: ~p~n", [State, Call, Res]),
	false.

%% === whatever ==========

get_map(#test_layer{map_id = MapId}, #state{maps = Maps}) ->
	lists:keyfind(MapId, #test_map.id, Maps).

layer_put_valid(Json, undefined) ->
	Name = proplists:get_value(<<"name">>, rpgb_requests_tests:fix_json_keys(Json)),
	if
		Name == undefined ->
			false;
		Name == <<>> ->
			false;
		true ->
			true
	end;
layer_put_valid(_,_) ->
	true.

extract_layer(Original, {map, Result}) ->
	KeyBase = [<<"layers">>, 1],
	extract_layer(Original, Result, KeyBase);

extract_layer(Original, Result) ->
	KeyBase = [],
	extract_layer(Original, Result, KeyBase).

extract_layer(Original, Result, KeyBase) ->
	IdKey = KeyBase ++ [<<"id">>],
	UrlKey = KeyBase ++ [<<"url">>],
	L1 = #test_layer{
		id = {call, rpgb_requests_tests, extract_json, [Result, IdKey]},
		url = {call, rpgb_requests_tests, extract_json, [Result, UrlKey]},
		properties = {call, rpgb_requests_tests, extract_json, [Result, KeyBase]}
	},
	case Original of
		undefined ->
			L1;
		_ ->
			L1#test_layer{
				id = Original#test_layer.id,
				map_id = Original#test_layer.map_id,
				map_url = Original#test_layer.map_url,
				url = Original#test_layer.url,
				zones = Original#test_layer.zones,
				auras = Original#test_layer.auras
			}
	end.
