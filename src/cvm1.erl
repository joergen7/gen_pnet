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

-module( cvm1 ).

-behaviour( gen_pnet ).

-export( [init/1, place_lst/0, trsn_lst/0, preset/1, enum_consume_map/3,
          fire/3] ).

%%====================================================================
%% gen_pnet callback functions
%%====================================================================

init( _UserArg ) -> {ok, #{}, []}.

place_lst() -> [coin_slot, compartment].

trsn_lst() -> [t].

preset( t ) -> [coin_slot].

enum_consume_map( t, #{ coin_slot := CLst }, _UserInfo ) ->
  [#{ coin_slot => [C] } || C <- CLst].

fire( t, _ConsumeLst, _UserInfo ) ->
  #{ compartment => [cookie_box] }.
