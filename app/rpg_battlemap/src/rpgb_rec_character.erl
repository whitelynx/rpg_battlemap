-module(rpgb_rec_character).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([make_json/1]).

make_json(Character) ->
	Url = rpgb:get_url(["characters", integer_to_list(Character#rpgb_rec_character.id)]),
	Owner = case Character#rpgb_rec_character.owner_id of
		undefined ->
			null;
		OwnerId ->
			case rpgb_data:get_by_id(rpgb_rec_user, OwnerId) of
				{ok, User} ->
					User#rpgb_rec_user.email;
				_ ->
					null
			end
	end,
	MutatorList = [{<<"url">>, Url}, owner_id, {<<"owner">>, Owner}],
	rpgb_rec_character:to_json(MutatorList, Character).
