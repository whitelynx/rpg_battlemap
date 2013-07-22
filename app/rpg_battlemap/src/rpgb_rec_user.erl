-module(rpgb_rec_user).

-compile([{parse_transform, rec2json}]).

-include("rpg_battlemap.hrl").

-export([get_maybe_created/1]).

get_maybe_created(Email) ->
	{ok, Re} = re:compile(".+@.+"),
	case re:run(Email, Re) of
		nomatch ->
			{error, not_email};
		_ ->
			case rpgb_data:search(rpgb_rec_user, [{email, Email}]) of
				{ok, []} ->
					User = #rpgb_rec_user{email = Email, name = Email},
					rpgb_data:save(User);
				{ok, [User | _]} ->
					{ok, User}
			end
	end.
