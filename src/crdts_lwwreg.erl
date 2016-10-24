-module(crdts_lwwreg).

-export([
   new/0
  ,update/2
  ,update/3
  ,value/1
  ,value/2
  ,descend/2
  ,join/2
]).

%%
%% create new data type value
new() ->
   {undefined, uid:encode(uid:g())}.

%%
%% update data type value
update(X, _Value) ->
   {X, uid:encode(uid:g())}.   

update(_Lens, X, _Value) ->
   {X, uid:encode(uid:g())}.

%%
%% query data type value
value({Value, _}) ->
   Value.

value(_Lens, Value) ->
   Value.

%%
%% compare values, return if A =< B in semi-lattice
%% LWW register requires a total order of assignments. 
%% Either timestamps or k-order values do not guarantee total order.
%% Let's approximate total order using neighborhood approximation.
descend({_, A}, {_, B}) ->
   A =< B.

%%
%% merge two value(s)
join({_, Ta} = A, {_, Tb} = B) ->
   case Ta =< Tb of
      true  -> B;
      false -> A
   end.
