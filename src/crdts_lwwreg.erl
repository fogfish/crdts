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
%%   LWW Register
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

update(_, X, _Value) ->
   {X, uid:encode(uid:g())}.

%%
%% query data type value
value({Value, _}) ->
   Value.

value(_, Value) ->
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
