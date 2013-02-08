-module(rpgb_combatant).

-include("rpg_battlemap.hrl").

-export([make_json/4]).
-export([get_map_combatants/1]).
-export([delete/1]).

make_json(Proto, Host, Port, Combatant) ->
	Url = rpgb:get_url(Proto, Host, Port, ["map", 
		integer_to_list(Combatant#rpgb_rec_combatant.battlemap_id),
		"combatants", integer_to_list(Combatant#rpgb_rec_combatant.id)]),
	Combatant:to_json([{url, Url}]).

get_map_combatants(IntialId) ->
	Got = rpgb_data:get_by_id(rpgb_rec_combatant, IntialId),
	get_map_combatants(Got, []).

get_map_combatants({error, notfound}, Acc) ->
	lists:reverse(Acc);

get_map_combatants({ok, Combatant}, Acc) ->
	Got = rpgb_data:get_by_id(rpgb_rec_combatant, Combatant#rpgb_rec_combatant.next_combatant_id),
	get_map_combatants(Got, [Combatant | Acc]).

delete(#rpgb_rec_combatant{id = Id}) ->
	delete(Id);

delete(Id) ->
	rpgb_data:delete(rpgb_rec_combatant, Id).