%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% A generic Petri net OTP library
%%
%% Copyright 2016 Jörgen Brandt. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( gen_pnet ).

-export( [start_link/1, start_link/2, ls/2, call/2, cast/2, produce/2,
          produce_token_lst/3, produce_token/3] ).

%%====================================================================
%% Callback functions
%%====================================================================

-callback place_lst() ->
            [atom()].

-callback trsn_lst() ->
            [atom()].

-callback init_marking() ->
            pass | {produce, #{ atom() => [_] }}.

-callback preset( Trsn :: atom() ) ->
            [atom()].

-callback mode_lst( Trsn :: atom(), RelevantMap :: #{ atom() => [_] } ) ->
            [#{ atom() => [_] }].

-callback fire( Trsn :: atom(), ConsumeMap :: #{ atom() => [_] } ) ->
            pass | {produce, ProduceMap :: #{ atom() => [_] }}.

-callback handle_call( Request :: _, From :: {pid(), _} ) ->
            {pass, _} | {produce, #{ atom() => [_]}, _}.

-callback handle_cast( Request :: _ ) ->
            pass | {produce, #{ atom() => [_]}}.

%%====================================================================
%% API Functions
%%====================================================================

start_link( Mod ) ->
  gen_server:start_link( pnet_srv, {Mod, Mod}, [] ).

start_link( ServerName, Mod ) ->
  gen_server:start_link( ServerName, pnet_srv, {Mod, Mod}, [] ).

ls( Pid, Place ) ->
  pnet_srv:ls( Pid, Place ).

call( Pid, Request ) ->
  pnet_srv:call( Pid, Request ).

cast( Pid, Request ) ->
  pnet_srv:cast( Pid, Request ).

produce( Pid, ProdMap ) ->
  pnet_srv:produce( Pid, ProdMap ).

produce_token_lst( Pid, Place, TokenLst ) ->
  pnet_srv:produce_token_lst( Pid, Place, TokenLst ).

produce_token( Pid, Place, Token ) ->
  pnet_srv:produce_token( Pid, Place, Token ).




%%====================================================================
%% Internal Functions
%%====================================================================

-spec enum_mod( [atom()], #{ atom() => [_] } ) -> [#{ atom() => [_] }].

enum_mod( Preset, StateMap ) ->

  F = fun( P, Acc ) ->
        N = maps:get( P, Acc, 0 ),
        Acc#{ P => N+1 }
      end,

  % gather count map
  CountMap = lists:foldl( F, #{}, Preset ),

  G = fun( P, N, Acc ) ->
        #{ P := TkLst } = StateMap,
        Acc#{ P => lib_combin:cnr( N, TkLst ) }
      end,

  % enumerate drawing combinations for each preset place individually
  CmbMap = maps:fold( G, #{}, CountMap ),

  % enumerate permutations of map containing drawing combinations
  lib_combin:permut_map( CmbMap ).