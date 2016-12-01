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
%%   G-Counter test suite
-module(orsets_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).


%%
%% g-counter interface
-export([
   new/1, update/1, value/1, descend/1, join/1 
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, crdt}
   ].

groups() ->
   [
      {crdt, [parallel], 
         [new, update, value, descend, join]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   

new(_Config) ->
   {crdts_orsets, []} = crdts:new(orsets).

update(_Config) ->
   A = crdts:new(orsets),
   {crdts_orsets, [{a, [{_, false}]}]} = crdts:update(a, A),
   {crdts_orsets, [{a, [{_, true}]}]}  = crdts:update(remove, a, crdts:update(a, A)).

value(_Config) ->
   A = crdts:new(orsets),
   B = crdts:update(a, A),
   C = crdts:update(b, B),
   D = crdts:update(c, C),
   E = crdts:update(remove, c, D),
   F = crdts:update(remove, b, E),

   [] = crdts:value(A),
   [a] = crdts:value(B),
   [a,b] = crdts:value(C),
   [a,b,c] = crdts:value(D),
   [a,b] = crdts:value(E),
   [a] = crdts:value(F).

descend(_Config) ->
   A = crdts:new(orsets),
   B = crdts:update(a, A),
   C = crdts:update(b, B),
   D = crdts:update(c, C),
   E = crdts:update(remove, c, D),
   F = crdts:update(remove, b, E),

   true = crdts:descend(A, B),
   true = crdts:descend(B, C),
   true = crdts:descend(C, D),
   true = crdts:descend(D, E),
   true = crdts:descend(E, F),
   false= crdts:descend(F, B).

join(_Config) ->
   A  = crdts:new(orsets),
   Bx = crdts:update(a, A),
   By = crdts:update(b, A),
   C  = crdts:join(Bx, By),
   
   false= crdts:descend(Bx, By),
   false= crdts:descend(By, Bx),
   true = crdts:descend(Bx,  C),
   true = crdts:descend(By,  C),
   [a, b] = crdts:value(C).




