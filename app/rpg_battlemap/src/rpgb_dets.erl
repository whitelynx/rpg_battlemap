-module(rpgb_dets).
% TODO actually create the behavior
%-behavior(gen_server).
-behavior(rpgb_gen_data).

-include("log.hrl").
-include("rpg_battlemap.hrl").
-include_lib("stdlib/include/qlc.hrl").

-define(dets_table, rpgb_dets).

% api
-export([start_link/0, start_link/1, stop/0]).
-export([update_records/0]).
% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).
% rpgb_gen_data
-export([
  get_by_id/2,
  save/1,
  delete/2,
  search/2
]).

%% ====================================================================
%% External api
%% ====================================================================

start_link() ->
  start_link([]).

start_link(Options) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

stop() ->
   gen_server:cast(?MODULE, stop).

get_by_id(Type, Id) ->
  case dets:lookup(?dets_table, {Type, Id}) of
    [] -> {error, notfound};
    [{_Key, O} | _] -> {ok, O}
  end.

save(Rec) ->
  Rec1 = case element(2, Rec) of
    undefined ->
      NewId = dets:update_counter(?dets_table, element(1, Rec), 1),
      setelement(2, Rec, NewId);
    _ ->
      Rec
  end,
  Type = element(1, Rec1),
  Id = element(2, Rec1),
  case dets:insert(?dets_table, {{Type, Id}, Rec1}) of
    ok ->
      {ok, Rec1};
    Err ->
      Err
  end.

delete(Type, Id) ->
  case dets:delete(?dets_table, {Type, Id}) of
    ok ->
      {ok, 1};
    Err ->
      Err
  end.

search(Type, Params) ->
  FieldNames = get_field_names(Type),
  BlankTuple1 = ['_' || _ <- FieldNames],
  BlankTuple2 = [Type | BlankTuple1],
  BlankRec = list_to_tuple(BlankTuple2),
  FieldIndexs = indexize(FieldNames, 2),
  Match = build_match(Params, FieldIndexs, BlankRec),
  case dets:match_object(?dets_table, {{Type, '_'}, Match}) of
    Objs when is_list(Objs) ->
      Objs1 = [O || {_, O} <- Objs],
      {ok, Objs1};
    E ->
      E
  end.

%% @doc run after a record schema change. If the record schema schanges,
%% this will do a generic transform in an attmept to get it back to good.
%% this makes a few assumptions:
%% 1: all entries in the dets are a record.
%% 2: all records have an id as element 2, created as element last -1,
%%    and updated as element last
%% 3: all records have reasonable defaults.
%% 4: updates to records always happen by adding a feild just before
%%    created.
%% 5: records never get smaller
update_records() ->
  gen_server:call(?MODULE, update_records, infinity).

%% ====================================================================
%% gen_server
%% ====================================================================

%% --------------------------------------------------------------------
%% Init
%% --------------------------------------------------------------------

init(Options) ->
  DataDir = case proplists:get_value(data_dir, Options) of
    undefined ->
      Dir1 = filename:join(code:priv_dir(rpg_battlemap), "data"),
      ok = filelib:ensure_dir(Dir1),
      Dir1;
    Dir ->
      filename:join(Dir, "data")
  end,
  {ok, _} = dets:open_file(?dets_table, [{file, DataDir}]),
  Counters = [ rpgb_rec_user, rpgb_rec_user_group, rpgb_rec_battlemap,
    rpgb_rec_zone, rpgb_rec_combatant, rpgb_rec_character,
    rpgb_rec_layer ],
  [dets:insert_new(?dets_table, {C, 0}) || C <- Counters],
  {ok, undefined}.

%% --------------------------------------------------------------------
%% handle_call
%% --------------------------------------------------------------------

handle_call(update_records, _From, State) ->
  % I'm not worryied about blocking this server since it doesn't do
  % anything else.
  TraverseFun = fun is_current_record/1,
  Updated = dets:traverse(?dets_table, TraverseFun),
  [dets:insert(?dets_table, Update) || Update <- Updated],
  {reply, ok, State};

handle_call(_Msg, _From, State) ->
	{reply, {error, invalid}, State}.

%% --------------------------------------------------------------------
%% handle_cast
%% --------------------------------------------------------------------

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% handle_info
%% --------------------------------------------------------------------

handle_info(_Msg, State) ->
	{noreply, State}.

%% --------------------------------------------------------------------
%% termiante
%% --------------------------------------------------------------------

terminate(_Meh, _State) ->
	dets:close(?dets_table).

%% --------------------------------------------------------------------
%% code_change
%% --------------------------------------------------------------------

code_change(_Meh, State, _Xtra) ->
	{ok, State}.

%% ====================================================================
%% internal
%% ====================================================================

get_field_names(rpgb_rec_user) -> record_info(fields, rpgb_rec_user);
get_field_names(rpgb_rec_user_group) -> record_info(fields, rpgb_rec_user_group);
get_field_names(rpgb_rec_battlemap) -> record_info(fields, rpgb_rec_battlemap);
get_field_names(rpgb_rec_layer) -> record_info(fields, rpgb_rec_layer);
get_field_names(rpgb_rec_zone) -> record_info(fields, rpgb_rec_zone);
get_field_names(rpgb_rec_combatant) -> record_info(fields, rpgb_rec_combatant);
get_field_names(rpgb_rec_character) -> record_info(fields, rpgb_rec_character).

indexize(Items, StartIndex) ->
  indexize(Items, StartIndex, []).

indexize([], _Index, Acc) ->
  lists:reverse(Acc);

indexize([Head | Tail], Index, Acc) ->
  indexize(Tail, Index + 1, [{Head, Index} | Acc]).

build_match([], _Indexs, Rec) ->
  Rec;
build_match([{Key, Value} | Tail], Indexes, Rec) ->
  case proplists:get_value(Key, Indexes) of
    undefined ->
      build_match(Tail, Indexes, Rec);
    N ->
      Rec1 = setelement(N, Rec, Value),
      build_match(Tail, Indexes, Rec1)
  end.

is_current_record(Object) when is_atom(element(1, Object)) ->
  % it's a counter, don't muck with it.
  continue;
is_current_record(Object) ->
  {{Type, _Id} = Key, Value} = Object,
  Fields = get_field_names(Type),
  if
    length(Fields) == size(Value) ->
      continue;
    true ->
      Default = create_default(Type),
      Updated = update_old_record(Value, Default),
      {continue, {Key, Updated}}
  end.

create_default(rpgb_rec_user) -> #rpgb_rec_user{};
create_default(rpgb_rec_user_group) -> #rpgb_rec_user_group{};
create_default(rpgb_rec_battlemap) -> #rpgb_rec_battlemap{};
create_default(rpgb_rec_layer) -> #rpgb_rec_layer{};
create_default(rpgb_rec_zone) -> #rpgb_rec_zone{};
create_default(rpgb_rec_combatant) -> #rpgb_rec_combatant{};
create_default(rpgb_rec_character) -> #rpgb_rec_character{}.

update_old_record(Old, Default) when is_tuple(Old), is_tuple(Default) ->
  OldList = tuple_to_list(Old),
  DefaultList = tuple_to_list(Default),
  {OldHead, OldTimes} = lists:split(length(OldList) - 2, OldList),
  {DefHead, _DefTimes} = lists:split(length(DefaultList) - 2, DefaultList),
  {_AlreadySet, NewFields} = lists:split(length(OldHead), DefHead),
  NewList = OldHead ++ NewFields ++ OldTimes,
  list_to_tuple(NewList).
