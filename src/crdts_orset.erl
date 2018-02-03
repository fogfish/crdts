%% @doc
%%   OR-Set
-module(crdts_orset).
-compile({parse_transform, category}).

-export([
   %% crdts interface
   new/0,
   value/1,
   insert/2,
   remove/2,
   has/2,
   descend/2,
   join/2,

   %%
   foreach/2,
   filter/2,
   map/2,
   fold/3
]).

%%
%%
-record(orset, {set}).

%%
%%
-spec new() -> crdts:orset().

new() ->
   #orset{set = orddict:new()}.

%%
%%
-spec value(crdts:orset()) -> _.

value(#orset{set = Set}) ->
   ordsets:from_list(   
      orddict:fetch_keys(
         orddict:filter(fun is_unique_exists/2, Set)
      )
   ).

is_unique_exists(_, Token) ->
   orddict:fold(fun(_, false, Acc) -> Acc + 1; (_, _, Acc) -> Acc end, 0, Token) > 0.   


%%
%%
-spec insert(_, crdts:orset()) -> crdts:orset().

insert(E, #orset{set = Set}) ->
   #orset{
      set = orddict:update(E, fun add_unique_pair/1, def_unique_token(), Set)
   }.

add_unique_pair(Unique) ->
   orddict:store(get_unique_token(), false, Unique).

get_unique_token() ->
   uid:encode(uid:g()).

def_unique_token() ->
   [{get_unique_token(), false}].

%%
%%
-spec remove(_, crdts:orset()) -> crdts:orset().

remove(E, #orset{set = Set} = OrSet) ->
   case orddict:is_key(E, Set) of
      false -> 
         OrSet;
      true  -> 
         #orset{
            set = orddict:update(E, fun rmv_unique_pair/1, Set)
         }
   end.

rmv_unique_pair(Unique) ->
   orddict:map(fun(_, _) -> true end, Unique).

%%
%%
-spec has(_, crdts:orset()) -> true | false.

has(E, OrSet) ->
   ordsets:is_element(E, value(OrSet)).


%%
%% compare values, return if A =< B in semi-lattice
-spec descend(crdts:orset(), crdts:orset()) -> true | false.

descend(#orset{set = A}, #orset{set = B}) ->
   descend_orsets(A, B).

descend_orsets([{X, Ta} | A], [{X, Tb} | B]) ->
   case descend_tokens(Ta, Tb) of
      true  -> descend_orsets(A, B);
      false -> false
   end;

descend_orsets([{X,  _} | _], [{Y,  _} | _]) when X < Y ->
   % X do not exists at B, A is not a subset of B
   false;

descend_orsets(A, [_ | B]) ->
   % Y do not exists at A, A maybe is a subset of B
   descend_orsets(A, B);

descend_orsets([], _) ->
   % A is subset of B
   true;  

descend_orsets(_, []) ->
   % A is not a subset of B
   false.


descend_tokens([{X, true} | _], [{X, false} | _]) ->
   false;

descend_tokens([{X, _} | A], [{X, _} | B]) ->
   descend_tokens(A, B);
   
descend_tokens([{X, _} | _], [{Y, _} | _]) when X < Y ->
   % X do not exists at B, A is not a subset of B
   false;

descend_tokens(A, [_ | B]) ->
   % Y do not exists at A, A maybe is a subset of B
   descend_tokens(A, B);

descend_tokens([], _) ->
   % A is subset of B
   true;  

descend_tokens(_, []) ->
   % A is not a subset of B
   false.


%%
%% merges local state with the state of some remote replica
-spec join(crdts:orset(), crdts:orset()) -> crdts:orset().

join(#orset{set = A}, #orset{set = B}) ->
   #orset{
      set = orddict:merge(fun join_token/3, A, B)
   }.

join_token(_, A, B) ->
   orddict:merge(fun(_, X, Y) -> X or Y end, A, B).


%%
%%
-spec foreach(fun((_) -> ok), crdts:orset()) -> ok.

foreach(Fun, #orset{set = Set}) ->
   lists:foreach(
      fun({E, _Unique}) -> Fun(E) end,
      Set
   ).


%%
%%
-spec filter(fun((_) -> true | false), crdts:orset()) -> crdts:orset().

filter(Fun, #orset{set = Set}) ->
   #orset{
      set = orddict:filter(
         fun(E, _Unique) -> Fun(E) end,
         Set
      )
   }.


%%
%%
-spec map(fun((_) -> _), crdts:orset()) -> crdts:orset().

map(Fun, #orset{set = Set}) ->
   #orset{
      set = lists:map(
         fun({E, Unique}) -> {Fun(E), Unique} end,
         Set
      )
   }.


%%
%%
-spec fold(fun((_) -> _), _, crdts:orset()) -> crdts:orset().

fold(Fun, Acc0, #orset{set = Set}) ->
   orddict:fold(
      fun(E, _Unique, Acc) -> Fun(E, Acc) end,
      Acc0,
      Set
   ).

