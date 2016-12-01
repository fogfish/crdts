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
%%   OR-Sets
-module(crdts_orsets).

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
   orddict:new().

%%
%% update data type value
update(X, Value) ->
   % default is put lens
   update(add, X, Value).


update(add, X, Value) ->
   orddict:update(X, fun token_add/1, token_new(), Value);
   
update(remove, X, Value) ->
   case orddict:is_key(X, Value) of
      false -> Value;
      true  -> orddict:update(X, fun token_rmv/1, Value)
   end.

%%
%% query data type value
value(Value) ->
   orddict:fetch_keys(
      orddict:filter(fun token_isa/2, Value)
   ).

value(Lens, Value) ->
   lens:get(Lens, value(Value)).

%%
%% compare values, return if A =< B in semi-lattice
descend([{X, Ta} | A], [{X, Tb} | B]) ->
   % compare tokens
   case token_descend(Ta, Tb) of
      true  -> descend(A, B);
      false -> false
   end;

descend([{X,  _} | _], [{Y,  _} | _]) when X < Y ->
   % X do not exists at B, A is not a subset of B
   false;

descend(A, [_ | B]) ->
   % Y do not exists at A, A maybe is a subset of B
   descend(A, B);

descend([], _) ->
   % A is subset of B
   true;  

descend(_, []) ->
   % A is not a subset of B
   false.


%%
%% merge two value(s)
join(A, B) ->
   orddict:merge(fun token_join/3, A, B).


%%
%% token management
token() ->
   uid:encode(uid:g()).

%%
%%
token_new() ->
   [{token(), false}].

%%
%%
token_add(Token) ->
   orddict:store(token(), false, Token).

%%
%%
token_rmv(Token) ->
   orddict:map(fun(_, _) -> true end, Token).

%%
%%
token_isa(_, Token) ->
   orddict:fold(fun(_, false, Acc) -> Acc + 1; (_, _, Acc) -> Acc end, 0, Token) > 0.   

%%
%%
token_join(_, A, B) ->
   orddict:merge(fun(_, X, Y) -> X or Y end, A, B).

%%
%%
token_descend([{X, true} | _], [{X, false} | _]) ->
   false;

token_descend([{X, _} | A], [{X, _} | B]) ->
   token_descend(A, B);
   
token_descend([{X, _} | _], [{Y, _} | _]) when X < Y ->
   % X do not exists at B, A is not a subset of B
   false;

token_descend(A, [_ | B]) ->
   % Y do not exists at A, A maybe is a subset of B
   token_descend(A, B);

token_descend([], _) ->
   % A is subset of B
   true;  

token_descend(_, []) ->
   % A is not a subset of B
   false.

