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

-export( [init/1, place_lst/0, trsn_lst/0, preset/1, enum_consume_map/3,
          fire/3] ).

init( _InitArg ) ->
  InitMarking = #{ storage => [cookie_box, cookie_box, cookie_box] },
  {ok, InitMarking, []}.

place_lst() ->
  [coin_slot, cash_box, signal, storage, compartment].

trsn_lst() -> [a, b].

preset( a ) -> [coin_slot];
preset( b ) -> [signal, storage].

enum_consume_map( a, #{ coin_slot := CLst }, _UserInfo ) ->
  [#{ coin_slot => [C] } || C <- CLst];

enum_consume_map( b, #{ signal := SgLst, storage := StLst }, _UserInfo ) ->
  [#{ signal => [Sg], storage => [St] } || Sg <- SgLst, St <- StLst].

fire( a, _ConsumeMap, _UserInfo ) ->
  #{ signal => [sig], cash_box => [coin] };

fire( b, _ConsumeMap, _UserInfo ) ->
  #{ compartment => [cookie_box]}.
