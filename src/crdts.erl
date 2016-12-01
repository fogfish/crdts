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
%%   convergent replicated data types
-module(crdts).

-export([
   new/1
  ,update/2
  ,update/3
  ,value/1
  ,value/2
  ,descend/2
  ,join/2
]).

%%%------------------------------------------------------------------
%%%
%%% data types
%%%
%%%------------------------------------------------------------------
-export_type([crdt/0]).

%%
%%
-type crdt()     :: {type(), s()}.
-type type()     :: gcounter
                  | gsets
                  | lwwreg.

-type lens()     :: atom().

-type s()        :: _. %% type of crdt object
-type a()        :: _. %% type of focused element (focus type)  



%%%------------------------------------------------------------------
%%%
%%% data types
%%%
%%%------------------------------------------------------------------

%%
%% create new data type value
-spec new(type()) -> crdt().

new(gcounter) -> new(crdts_gcounter);
new(gsets)    -> new(crdts_gsets);
new(orsets)   -> new(crdts_orsets);
new(lwwreg)   -> new(crdts_lwwreg);
new(CRDT)     -> {CRDT, CRDT:new()}.

%%
%% writes to the replica state in accordance with data type restrictions
-spec update(a(), crdt()) -> crdt().
-spec update(lens(), a(), crdt()) -> crdt().

update(X, {CRDT, Value}) -> 
   {CRDT, CRDT:update(X, Value)};
update(_, undefined) ->
   undefined.

update(Lens, X, {CRDT, Value}) -> 
   {CRDT, CRDT:update(Lens, X, Value)};
update(_, _, undefined) ->
   undefined.

%%
%% reads the state of the replica, with no side effects
-spec value(crdt()) -> crdt().
-spec value(lens(), crdt()) -> crdt().

value({CRDT, Value}) -> 
   CRDT:value(Value);
value(undefined) ->
   undefined.

value(Lens, {CRDT, Value}) -> 
   CRDT:value(Lens, Value);
value(_Lens, undefined) ->
   undefined.

%%
%% compare values, return if A =< B in semi-lattice
-spec descend(crdt(), crdt()) -> true | false.

descend({CRDT, ValueA}, {CRDT, ValueB}) -> 
   CRDT:descend(ValueA, ValueB);
descend(undefined, _) ->
   true;
descend(_, undefined) ->
   false.

%%
%% merges local state with the state of some remote replica
-spec join(crdt(), crdt()) -> crdt().

join({CRDT, ValueA}, {CRDT, ValueB}) -> 
   {CRDT, CRDT:join(ValueA, ValueB)};
join(undefined, B) ->
   B;
join(A, undefined) ->
   A.


