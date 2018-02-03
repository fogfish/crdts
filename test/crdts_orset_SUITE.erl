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
%%   OR-Set test suite
-module(crdts_orset_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([all/0]).

%%
%% g-counter interface
-export([
   new/1,
   value/1,
   insert/1,
   remove/1,
   has/1,
   descend/1,
   join/1,
   foreach/1,
   filter/1,
   map/1,
   fold/1
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      new,
      value,
      insert,
      remove,
      has,
      descend,
      join,
      foreach,
      filter,
      map,
      fold
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   

new(_) ->
   {orset, []} = crdts_orset:new().

insert(_) ->
   A = crdts_orset:new(),
   {orset, [{a, [{_, false}]}]} = crdts_orset:insert(a, A).

remove(_) ->
   A = crdts_orset:new(),
   {orset, [{a, [{_, false}]}]} = crdts_orset:insert(a, A),
   {orset, [{a, [{_, true}]}]}  = crdts_orset:remove(a, crdts_orset:insert(a, A)).

has(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(a, A),
   C = crdts_orset:insert(b, B),
   D = crdts_orset:insert(c, C),
   E = crdts_orset:remove(c, D),

   true  = crdts_orset:has(a, E),
   false = crdts_orset:has(c, E).


value(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(a, A),
   C = crdts_orset:insert(b, B),
   D = crdts_orset:insert(c, C),
   E = crdts_orset:remove(c, D),
   F = crdts_orset:remove(b, E),

   [] = crdts_orset:value(A),
   [a] = crdts_orset:value(B),
   [a,b] = crdts_orset:value(C),
   [a,b,c] = crdts_orset:value(D),
   [a,b] = crdts_orset:value(E),
   [a] = crdts_orset:value(F).

descend(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(a, A),
   C = crdts_orset:insert(b, B),
   D = crdts_orset:insert(c, C),
   E = crdts_orset:remove(c, D),
   F = crdts_orset:remove(b, E),

   true = crdts_orset:descend(A, B),
   true = crdts_orset:descend(B, C),
   true = crdts_orset:descend(C, D),
   true = crdts_orset:descend(D, E),
   true = crdts_orset:descend(E, F),
   false= crdts_orset:descend(F, B).

join(_Config) ->
   A  = crdts_orset:new(),
   Bx = crdts_orset:insert(a, A),
   By = crdts_orset:insert(b, A),
   C  = crdts_orset:join(Bx, By),
   
   false= crdts_orset:descend(Bx, By),
   false= crdts_orset:descend(By, Bx),
   true = crdts_orset:descend(Bx,  C),
   true = crdts_orset:descend(By,  C),
   [a, b] = crdts_orset:value(C).


foreach(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(a, A),
   C = crdts_orset:insert(b, B),
   D = crdts_orset:insert(c, C),

   crdts_orset:foreach(fun(_) -> ok end, D).

filter(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(a, A),
   C = crdts_orset:insert(b, B),
   D = crdts_orset:insert(c, C),

   E = crdts_orset:filter(fun(X) -> X =:= b end, D),
   [b] = crdts_orset:value(E).


map(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(1, A),
   C = crdts_orset:insert(2, B),
   D = crdts_orset:insert(3, C),

   E = crdts_orset:map(fun(X) -> X * 10 end, D),
   [10, 20, 30] = crdts_orset:value(E).


fold(_) ->
   A = crdts_orset:new(),
   B = crdts_orset:insert(1, A),
   C = crdts_orset:insert(2, B),
   D = crdts_orset:insert(3, C),

   6 = crdts_orset:fold(fun(X, Acc) -> X + Acc end, 0, D).



