-module(crdts_gsets).

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
   ordsets:new().

%%
%% update data type value
update(X, Value) ->
   ordsets:add_element(X, Value).

update(_Lens, X, Value) ->
   ordsets:add_element(X, Value).

%%
%% query data type value
value(Value) ->
   ordsets:to_list(Value).

value(Lens, Value) ->
   lens:get(Lens, Value).   

%%
%% compare values, return if A =< B in semi-lattice
descend(A, B) ->
   ordsets:is_subset(A, B).

%%
%% merge two value(s)
join(A, B) ->
   ordsets:union(A, B).

