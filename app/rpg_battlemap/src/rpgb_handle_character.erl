-module(rpgb_handle_character).

-include("log.hrl").
-include("rpg_battlemap.hrl").

-export([get_routes/0]).

-export([init/3, rest_init/2, allowed_methods/2, is_authorized/2,
	forbidden/2, content_types_provided/2, to_json/2, to_html/2,
	content_types_accepted/2, from_json/2, delete_resource/2, generate_etag/2]).

-record(ctx, { hostport, session, character_id, character}).

get_routes() ->
	[
		<<"/characters">>,
		<<"/characters/:characterid">>
	].

init(_Protos, _Req, _HostPort) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, [HostPort]) ->
	{ok, Session, Req1} = rpgb_session:get_or_create(Req),
	{_Path, Req2} = cowboy_req:path(Req1),
	{CharId, Req3} = cowboy_req:binding(characterid, Req2),
	CharId1 = case CharId of
		undefined ->
			undefined;
		_ ->
			try list_to_integer(binary_to_list(CharId)) of
				N ->
					N
			catch
				'ERROR':{badarg, _} ->
					<<"bad character id, will 404 later">>
			end
	end,
	{ok, Req3, #ctx{hostport = HostPort, session = Session, character_id = CharId1}}.

allowed_methods(Req, #ctx{character_id = undefined} = Ctx) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"HEAD">>], Req, Ctx};

allowed_methods(Req, Ctx) ->
	{[<<"GET">>, <<"PUT">>, <<"HEAD">>, <<"DELETE">>], Req, Ctx}.

is_authorized(Req, #ctx{session = Session} = Ctx) ->
	case rpgb_session:get_user(Session) of
		undefined ->
			{{false, <<"persona">>}, Req, Ctx};
		_User ->
			{true, Req, Ctx}
	end.

forbidden(Req, #ctx{character_id = CharacterId, session = Session} = Ctx) ->
	User = rpgb_session:get_user(Session),
	{Method, Req2} = cowboy_req:method(Req),
	case CharacterId of
		undefined ->
			{false, Req2, Ctx};
		_ ->
			case rpgb_data:get_by_id(rpgb_rec_character, CharacterId) of
				{ok, #rpgb_rec_character{owner_id = undefined} = Character} when Method == <<"GET">> ->
					{false, Req2, Ctx#ctx{character = Character}};
				{ok, #rpgb_rec_character{owner_id = OwnId} = Character} when User#rpgb_rec_user.id == OwnId ->
					{false, Req2, Ctx#ctx{character = Character}};
				{ok, Character} ->
					{true, Req2, Ctx#ctx{character = Character}};
				{error, not_found} ->
					{ok, Req3} = cowboy_req:reply(404, Req2),
					{halt, Req3, Ctx}
			end
	end.

delete_resource(Req, #ctx{character_id = CharacterId} = Ctx) ->
	case rpgb_data:delete(rpgb_rec_character, CharacterId) of
		{ok, _} ->
			{true, Req, Ctx};
		{error, Err} ->
			lager:info("error deleting: ~p", [Err]),
			Body = iolist_to_binary(io_lib:format("Error deleteing:  ~p", [Err])),
			{ok, Req1} = cowboy_req:set_resp_body(Body, Req),
			{false, Req1, Ctx}
	end.

content_types_provided(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, to_json},
		{{<<"text">>, <<"html">>, '*'}, to_html}
	],
	{Types, Req, Ctx}.

content_types_accepted(Req, Ctx) ->
	Types = [
		{{<<"application">>, <<"json">>, '*'}, from_json}
	],
	{Types, Req, Ctx}.

to_json(Req, #ctx{character = undefined} = Ctx) ->
	User = rpgb_session:get_user(Ctx#ctx.session),
	{ok, Chars} = rpgb_data:search(rpgb_rec_character, [{owner_id, User#rpgb_rec_user.id}]),
	Json = lists:map(fun(C) ->
		rpgb_rec_character:make_json(C)
	end, Chars),
	{jsx:to_json(Json), Req, Ctx};

to_json(Req, #ctx{character = Character} = Ctx) ->
	%Url = make_location(Req, Ctx, Character),
	%Json = Character:to_json([{url, Url}]),
	Json = rpgb_rec_character:make_json(Character),
	{jsx:to_json(Json), Req, Ctx}.

to_html(Req, Ctx) ->
	rpgb:refresh_templates(base_dtl),
	User = rpgb_session:get_user(Ctx#ctx.session),
	LoginLink = rpgb:get_url(Req, ["account", "login"]),
	LogoutLink = rpgb:get_url(Req, ["account", "logout"]),
	{ok, Output} = base_dtl:render([{user, User},
		{login_link, LoginLink}, {logout_link, LogoutLink}]),
	{Output, Req, Ctx}.

from_json(Req, #ctx{character_id = CharacterId} = Ctx) ->
	#ctx{session = Session} = Ctx,
	User = rpgb_session:get_user(Session),
	InitialCharacter = case CharacterId of
		undefined ->
			#rpgb_rec_character{
				id = undefined,
				owner_id = User#rpgb_rec_user.id,
				created = os:timestamp(),
				updated = os:timestamp()
			};
		_ ->
			InitC = Ctx#ctx.character,
			InitC#rpgb_rec_character{updated = os:timestamp()}
	end,
	{ok, Body, Req1} = cowboy_req:body(Req),
	Term = jsx:to_term(Body),
	case validate_character(Term, InitialCharacter) of
		{ok, {_DerJson, Rec}} ->
			{ok, Rec2} = rpgb_data:save(Rec),
			%{Host, Port} = Ctx#ctx.hostport,
			OutJson = rpgb_rec_character:make_json(Rec2),
			Location = proplists:get_value(<<"url">>, OutJson),
			Req3 = cowboy_req:set_resp_body(jsx:to_json(OutJson), Req1),
			OutVal = case CharacterId of
				undefined ->
					{true, Location};
				_ ->
					true
			end,
			lager:info("output for from_json: ~p", [OutVal]),
			{OutVal, Req3, Ctx#ctx{character_id = Rec2#rpgb_rec_character.id, character = Rec2}};
		{error, State, ErrBody} ->
			ErrBody2 = jsx:to_json(ErrBody),
			Req2 = cowboy_req:set_resp_body(ErrBody2, Req1),
			{ok, Req3} = cowboy_req:reply(State, Req2),
			lager:info("halting due to from error: ~p", [ErrBody]),
			{halt, Req3, Ctx}
	end.

validate_character(Json, InitCharacter) ->
	ValidateFuns = [
		fun scrub_disallowed/1,
		fun check_blank_name/1,
		fun check_name_conflict/1,
		fun validate_json/1,
		fun check_named_character/1
	],
	rpgb:bind({Json, InitCharacter}, ValidateFuns).

check_named_character({Json, Character}) ->
	CharacterName = Character#rpgb_rec_character.name,
	JsonName = proplists:get_value(<<"name">>, Json),
	case {CharacterName, JsonName} of
		{undefined, undefined} ->
			{error, 422, <<"name cannot be blank.">>};
		_ ->
			{ok, {Json, Character}}
	end.

check_name_conflict({Json, Character}) ->
	#rpgb_rec_character{owner_id = Owner, name = CharacterName, id = Id} = Character,
	case proplists:get_value(<<"name">>, Json) of
		undefined ->
			{ok, {Json, Character}};
		CharacterName ->
			{ok, {Json, Character}};
		OtherName ->
			Searched = rpgb_data:search(rpgb_rec_character, [
				{name, OtherName}, {owner_id, Owner}]),
			case Searched of
				{ok, []} ->
					{ok, {Json, Character}};
				{ok, [#rpgb_rec_character{id = Id} | _]} ->
					{ok, {Json, Character}};
				Wut ->
					lager:info("name conflict got: ~p", [Wut]),
					{error, 409, <<"you already have a character by that name.">>}
			end
	end.

check_blank_name({Json, Character}) ->
	case proplists:get_value(<<"name">>, Json) of
		<<>> ->
			{error, 422, <<"name cannot be blank.">>};
		_ ->
			{ok, {Json, Character}}
	end.

scrub_disallowed({Json, Character}) ->
	{ok, Json2} = scrub_disallowed(Json),
	{ok, {Json2, Character}};

scrub_disallowed([{}]) ->
	{ok, [{}]};

scrub_disallowed(Json) ->
	Disallowed = [<<"id">>, <<"owner_id">>, <<"created">>, <<"updated">>],
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

validate_json({Json, Character}) ->
	case validate_json(Json, Character) of
		{ok, Character2} ->
			{ok, {Json, Character2}};
		{error, {bad_color, Key}} ->
			lager:info("invalid color: ~p", [Key]),
			Body = iolist_to_binary(io_lib:format("invalid color for ~s.", [Key])),
			Status = 422,
			{error, Status, Body}
	end;

validate_json(Json) ->
	case rpgb_rec_character:from_json(Json) of
		{ok, Rec, Warnings} ->
			validate_warnings(Warnings, Rec);
		Else ->
			Else
	end.

validate_json(Json, Rec) ->
	{ok, Json1} = scrub_disallowed(Json),
	case rpgb_rec_character:from_json(Rec, Json1) of
		{ok, Rec1, Warnings} ->
			validate_warnings(Warnings, Rec1);
		Else ->
			Else
	end.

validate_warnings([], Rec) ->
	{ok, Rec};

validate_warnings([color | Tail], Rec) ->
	Color = Rec#rpgb_rec_character.color,
	case validate_color(Color) of
		true ->
			validate_warnings(Tail, Rec);
		false ->
			{error, {bad_color, color}}
	end.

validate_color(Color) when is_binary(Color) ->
	true;
validate_color([_R, _G, _B] = Color) ->
	lists:all(fun erlang:is_integer/1, Color);
validate_color([R, G, B, A]) when A =< 1 andalso A >= 0 ->
	validate_color([R, G, B]);
validate_color(_) ->
	false.

generate_etag(Req, #ctx{character_id = undefined} = Ctx) ->
	{undefined, Req, Ctx};
generate_etag(Req, #ctx{character = Character} = Ctx) ->
	Bin = jsx:to_json(Character:to_json()),
	Updated = Character#rpgb_rec_character.updated,
	Bin2 = term_to_binary({Bin, Updated}),
	Md5 = crypto:md5(Bin2),
	Etag = rpgb_util:bin_to_hexstr(Md5),
	{{weak, list_to_binary(Etag)}, Req, Ctx}.
