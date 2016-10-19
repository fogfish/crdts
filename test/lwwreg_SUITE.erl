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
-module(lwwreg_SUITE).
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
   {crdts_lwwreg, {undefined, _}} = crdts:new(lwwreg).

update(_Config) ->
   {crdts_lwwreg, {a, _}} = crdts:update(a, crdts:new(lwwreg)).

value(_Config) ->
   A = crdts:new(lwwreg),
   B = crdts:update(a, A),
   C = crdts:update(b, B),
   D = crdts:update(c, C),

   undefined = crdts:value(A),
   a = crdts:value(B),
   b = crdts:value(C),
   c = crdts:value(D).

descend(_Config) ->
   A = crdts:new(lwwreg),
   B = crdts:update(a, A),
   C = crdts:update(b, B),
   D = crdts:update(c, C),

   true = crdts:descend(A, B),
   true = crdts:descend(B, C),
   true = crdts:descend(C, D),
   false= crdts:descend(D, A).

join(_Config) ->
   A  = crdts:new(lwwreg),
   Bx = crdts:update(a, A),
   By = crdts:update(b, A),
   C  = crdts:join(Bx, By),
   
   true = crdts:descend(Bx, By),
   false= crdts:descend(By, Bx),
   true = crdts:descend(Bx,  C),
   true = crdts:descend(By,  C),
   b = crdts:value(C).




