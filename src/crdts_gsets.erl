%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @description
%%   G-Sets
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
   update(add, X, Value).

update(add, X, Value)
 when is_list(X) ->
   ordsets:union(ordsets:from_list(X), Value);

update(add, X, Value) ->
   ordsets:add_element(X, Value).

%%
%% query data type value
value(Value) ->
   ordsets:to_list(Value).

value({has, X}, Value) ->
   ordsets:is_element(X, Value).   

%%
%% compare values, return if A =< B in semi-lattice
descend(A, B) ->
   ordsets:is_subset(A, B).

%%
%% merge two value(s)
join(A, B) ->
   ordsets:union(A, B).

