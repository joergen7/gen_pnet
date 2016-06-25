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

-include( "pnet.hrl" ).

-callback hot_transition_lst() ->
  TransitionLst::[atom()].

-callback in_transition_lst() -> TransitionLst::[atom()].

-callback mode_lst( Transition::atom(), State::_ ) ->
  ModeLst::[lst_map()].

-callback arc_labeling( Begin::atom(), End::atom() ) ->
  Labeling::atom().

-callback post_map( Transition::atom(), Mode::lst_map() ) ->
  PostMap::lst_map().

-callback init( Args::_ ) -> {ok, State:_}.
-callback place_add( Place::atom(), Token::_ ) -> ok.
-callback place_remove( Place::atom(), Token::_ ) -> ok.

%%====================================================================
%% gen_server API functions
%%====================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.


handle_call( {add_out_transition, Place, NetPid}, _From, OutTransitionLst )
when is_atom( Place ),
     is_pid( NetPid ) orelse is_atom( NetPid ) ->

  Ot = #out_tranition{ place = Place, net_pid = NetPid },

  {reply, ok, [Ot|OutTransitionLst]}.


handle_cast( _Request, State ) -> {noreply, State}.

handle_info( _Info, State ) -> {noreply, State}.

init( [] ) -> {ok, []}.

terminate( _Reason, _State ) -> ok.

%%====================================================================
%% API functions
%%====================================================================




%%====================================================================
%% Internal functions
%%====================================================================
