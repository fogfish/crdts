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
   {undefined, uid:g()}.

%%
%% update data type value
update(X, _Value) ->
   {X, uid:g()}.   

update(_Lens, X, _Value) ->
   {X, uid:g()}.

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
   compare(A, B).

-define(NEIGHBOR, 600000). %% 600 seconds

compare({uid, Node, _, _} = A, {uid, Node,  _, _} = B) ->
   A =< B;

compare({uid, _, _, _} = A, {uid, _, _, _} = B) ->
   case uid:t( uid:d(A, B) ) of
      X when X >= -?NEIGHBOR, X =< ?NEIGHBOR ->
         A =< B;
      X when X > ?NEIGHBOR ->
         true;
      _ ->
         false 
   end.

%%
%% merge two value(s)
join({_, Ta} = A, {_, Tb} = B) ->
   case compare(Ta, Tb) of
      true  -> B;
      false -> A
   end.
