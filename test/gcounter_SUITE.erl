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
-module(gcounter_SUITE).
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
   {crdts_gcounter, []} = crdts:new(gcounter).

update(_Config) ->
   Node = erlang:node(),
   {crdts_gcounter, [{Node, 1}]} = crdts:update(1, crdts:new(gcounter)).

value(_Config) ->
   A = crdts:new(gcounter),
   B = crdts:update(1,  A),
   C = crdts:update(10, B),
   D = crdts:update(100,C),

   0 = crdts:value(A),
   1 = crdts:value(B),
   11= crdts:value(C),
   111 = crdts:value(D).

descend(_Config) ->
   A = crdts:new(gcounter),
   B = crdts:update(1,  A),
   C = crdts:update(10, B),
   D = crdts:update(100,C),

   true = crdts:descend(A, B),
   true = crdts:descend(B, C),
   true = crdts:descend(C, D),
   false= crdts:descend(D, A).

join(_Config) ->
   A  = crdts:new(gcounter),
   Bx = crdts:update(x,  1, A),
   By = crdts:update(y, 10, A),
   C  = crdts:join(Bx, By),
   
   false= crdts:descend(Bx, By),
   false= crdts:descend(By, Bx),
   true = crdts:descend(Bx,  C),
   true = crdts:descend(By,  C),
   11 = crdts:value(C).




