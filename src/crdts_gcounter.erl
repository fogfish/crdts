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
%%   G-Counter
-module(crdts_gcounter).

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
   [].

%%
%% update data type value
update(X, Value) ->
   update(lens:pair(erlang:node(), 0), X, Value).

update(Lens, X, Value) ->
   lens:apply(Lens, fun(Y) -> Y + X end, Value).

%%
%% query data type value
value(Value) ->
   lists:sum([X || {_, X} <- Value]).

value(_Lens, Value) ->
   value(Value).

%%
%% compare values, return if A =< B in semi-lattice
descend([], _) ->
   true;
descend([{Node, _} = X | A], B) ->
   case lists:keyfind(Node, 1, B) of
      false ->
         false;
      Y  ->
         (X =< Y) andalso descend(A, B)
   end.   

%%
%% merge two value(s)
join(A, B) ->
   join1(lists:keysort(1, A), lists:keysort(1, B)).   

join1([{NodeA, _} = X | A], [{NodeB, _} | _] = B)
 when NodeA < NodeB ->
   [X | join1(A, B)];

join1([{NodeA, _} | _] = A, [{NodeB, _} = X | B])
 when NodeA > NodeB ->
   [X | join1(A, B)];

join1([{Node, X} | A], [{Node, Y} | B]) ->
   [{Node, erlang:max(X, Y)} | join1(A, B)];

join1([], B) ->
   B;

join1(A, []) ->
   A.

