%% -*- erlang -*-
%%
%% pnet: A Petri net emulator.
%%
%% Copyright 2016 Jörgen Brandt, Marc Bux, and Ulf Leser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( pnet ).

%%%
%%%

-behaviour( gen_server ).

%% gen_server API exports
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).

%% API exports
-export( [start_link/1] ).

-include( "pnet.hrl" ).

-record( mod_state, { mod, user_state, cold_transition_lst = [] } ).

-callback init() ->
  {ok, UserState::_}.

-callback hot_transition_lst() ->
  TransitionLst::[atom()].

-callback consume( Transition::atom(), UserInfo::_ ) ->
  ConsumeMapLst::[#{ atom() => [_] }].

-callback produce( Transition::atom(), ConsumeMap::#{ atom() => [_] } ) ->
  ProduceMap::#{ atom() => [_] }.

-callback place_add( Place::atom(), TokenLst::[_], UserState::_ ) ->
  {ok, NewUserState::_}.

-callback place_remove( Place::atom(), TokenLst::[_], UserState::_ ) ->
  {ok, NewUserState::_}.

-callback place_get( Place::atom(), UserInfo::_ ) -> TokenLst::[_].


%%====================================================================
%% gen_server API functions
%%====================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.



handle_call( _Request, _From, State ) -> {reply, ok, State}.


handle_cast( _Request, State ) -> {noreply, State}.


handle_info( Ct = #cold_transition{}, ModState = #mod_state{ mod = Mod, cold_transition_lst = ColdTransitionLst } )
when is_atom( Mod ),
     is_list( ColdTransitionLst ) ->

  ModState1 = ModState#mod_state{ cold_transition_lst = [Ct|ColdTransitionLst] },
  {ok, ModState2} = process_cold_transitions( Mod, ModState1 ),

  {reply, ok, ModState2};

handle_info( #add_token{ place = Place, token = Token}, ModState = #mod_state{ mod = Mod, user_state = UserState } )
when is_atom( Place ),
     is_atom( Mod ) ->

  {ok, UserState1} = apply( Mod, place_add, [Place, Token, UserState] ),
  {ok, UserState2} = eval_pnet( Mod, UserState1 ),
  {ok, UserState3} = process_cold_transitions( Mod, UserState2 ),

  {noreply, ModState#mod_state{ user_state = UserState3 }}.



init( Mod ) ->

  {ok, UserState1} = apply( Mod, init, [] ),
  {ok, UserState2} = eval_pnet( Mod, UserState1 ),

  {ok, #mod_state{ mod = Mod, user_state = UserState2}}.



terminate( _Reason, _State ) -> ok.

%%====================================================================
%% API functions
%%====================================================================

start_link( Mod ) when is_atom( Mod ) ->
  gen_server:start_link( ?MODULE, Mod, [] ).


%%====================================================================
%% Internal functions
%%====================================================================

eval_pnet( _Mod, UserState ) -> {ok, UserState}.






process_cold_transitions( _Mod, UserState ) -> {ok, UserState}.