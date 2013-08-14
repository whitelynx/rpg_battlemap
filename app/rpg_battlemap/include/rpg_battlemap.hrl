% Define the data structures used throughout the app.

-type(time() :: {pos_integer(), non_neg_integer(), non_neg_integer()} | 'undefined').
-type(keyval() :: {binary(), any()}).

-record(rpgb_rec_user, {
	id :: 'undefined' | pos_integer(),
	email :: binary(),
	name :: binary(),
	group_id = 1 :: pos_integer(),
	permissions = [] :: [atom()],
	max_maps = 10 :: pos_integer() | 'infinity',
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_user_group, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	permissions = [] :: [atom()],
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_battlemap, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	owner_id :: pos_integer(),
	participant_ids = [] :: [pos_integer()],
	rating = g :: g | pg | r | x,
	zoom  = 1 :: float(),
	translate_x = 0 :: integer(),
	translate_y = 0 :: integer(),
	grid_spacing = 32 :: pos_integer(),
	background_color = <<"#888888">> :: binary(),
	gridline_color = <<"#000000">> :: binary(),
	grid_opacity = 0.5 :: float(),
	layer_ids = [] :: [pos_integer()], % list is bottom up order
	combatant_ids = [] :: [pos_integer()],
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_layer, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	battlemap_id :: pos_integer(),
	zone_ids = [] :: [pos_integer()],
	scenery_ids = [] :: [pos_integer()],
	aura_ids = [] :: [pos_integer()],
	created :: time(),
	updated :: time()
}).

-record(rpgb_point, {
	x = 0 :: float(),
	y = 0 :: float()
}).

-record(rpgb_rec_zone, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	type = 'zone' :: 'zone' | 'scenery' | 'aura',
	layer_id :: pos_integer(),
	rotation = 0 :: float(),
	stroke_color = <<"#000000">> :: binary(),
	stroke_width = 5 :: non_neg_integer(),
	stroke_opacity = 1 :: float(),
	fill_color = <<"008800">> :: binary(),
	fill_opacity = 1 :: float(),
	shape = 'rect' :: 'rect' | 'circle' | 'ellipse' | 'line' | 'polyline' | 'polygon' | 'path',
	% polygon, polyline
	points = [] :: [#rpgb_point{}],
	% rect
	x :: float(),
	y :: float(),
	width :: float(),
	height :: float(),
	% rect, ellipse
	rx :: float(),
	ry :: float(),
	% circle, ellipse
	cx :: float(),
	cy :: float(),
	% cirlce
	r :: float(),
	% line
	x1 :: float(),
	y1 :: float(),
	x2 :: float(),
	y2 :: float(),
	% path nyi fully.
	d,
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_combatant, {
	id :: 'undefined' | pos_integer(),
	name :: binary(),
	battlemap_id :: pos_integer(),
	owner_id :: pos_integer(),
	invisible = false :: boolean(),
	dead = false :: boolean(),
	color = <<"green">> :: binary(),
  portrait_image :: 'undefined' | binary(),
	token_image :: 'undefined' | binary(),
	x = 0 :: integer(),
	y = 0 :: integer(),
  layer_id :: 'undefined' | pos_integer(),
	initiative = 1 :: number(),
	size = 1 :: pos_integer(),
	aura_size = 0 :: non_neg_integer(),
	aura_color :: 'undefined' | binary(),
	created :: time(),
	updated :: time()
}).

-record(rpgb_rec_character, {
  id :: 'undefined' | pos_integer(),
  owner_id :: 'undefined' | pos_integer(),
	name :: binary(),
  color = <<"green">> :: binary(),
  portrait_image_url :: 'undefined' | binary(),
  token_image_url :: 'undefined' | binary(),
  size = 1 :: pos_integer(),
  public = false :: boolean(),
	created :: time(),
	updated :: time()
}).
