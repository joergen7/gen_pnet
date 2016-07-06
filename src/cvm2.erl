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

-module( cvm2 ).

-behaviour( gen_pnet ).

-export( [init/1, place_lst/0, trsn_lst/0, preset/1, enum_consume_lst/3,
          fire/3] ).

-include( "include/gen_pnet.hrl" ).

init( _InitArg ) -> {ok, []}.

place_lst() -> [coin_slot, cash_box, signal, storage, compartment].

trsn_lst() -> [a, b].

preset( a ) -> [coin_slot];
preset( b ) -> [signal].

enum_consume_lst( a, #{ coin_slot := CsLst }, _UserInfo ) ->
  [[C] || C <- CsLst];

enum_consume_lst( b, #{ signal := SgLst, storage := StLst }, _UserInfo ) ->
  [[Sg, St] || Sg <- SgLst, St <- StLst].

fire( a, [#token{ place = coin_slot }], _UserInfo ) ->
  [#token{ place = signal }, #token{ place = cash_box }];

fire( b, [#token{ place = signal }, #token{ place = storage }], _UserInfo ) ->
  [#token{ place = compartment }].
