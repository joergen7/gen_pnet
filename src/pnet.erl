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
-behaviour( gen_server ).

%%%
%%%


%%====================================================================
%% Exports
%%====================================================================

%% gen_server API exports
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2] ).

%% API exports
-export( [start_link/1] ).

%%====================================================================
%% Includes
%%====================================================================


-include( "pnet.hrl" ).

%%====================================================================
%% Internal record definitions
%%====================================================================


-record( mod_state, { mod, user_state, cold_transition_lst = [] } ).
-record( step_result, { has_changed = false, user_state} ).


%%====================================================================
%% Callback functions
%%====================================================================

-callback init() ->
  UserState::_.

-callback hot_transition_lst() ->
  TransitionLst::[atom()].

-callback consume_map_lst( Transition::atom(), UserState::_ ) ->
  ConsumeMapLst::[#{ atom() => [_] }].

-callback produce_map( Transition::atom(), ConsumeMap::#{ atom() => [_] } ) ->
  ProduceMap::#{ atom() => [_] }.

-callback place_add( Place::atom(), TokenLst::[_], UserState::_ ) ->
  NewUserState::_.

-callback place_remove( Place::atom(), TokenLst::[_], UserState::_ ) ->
  NewUserState::_.

-callback place_get( Place::atom(), UserState::_ ) -> TokenLst::[_].


%%====================================================================
%% gen_server API functions
%%====================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_call( _Request, _From, State ) -> {reply, ok, State}.
handle_cast( _Request, State ) -> {noreply, State}.
terminate( _Reason, _State ) -> ok.


handle_info( Ct = #cold_transition{},
             ModState = #mod_state{ mod = Mod,
                                    user_state = UserState,
                                    cold_transition_lst = ColdTransitionLst } )
when is_list( ColdTransitionLst ) ->

  ColdTransitionLst1 = [Ct|ColdTransitionLst],
  UserState1 = eval_cold( Mod, UserState, ColdTransitionLst1 ),

  ModState1 = ModState#mod_state{ user_state = UserState1,
                                  cold_transition_lst = ColdTransitionLst1 },

  {noreply, ModState1};

handle_info( #add_token{ place = Place, token = Token},
             ModState = #mod_state{ mod = Mod,
                                    user_state = UserState,
                                    cold_transition_lst = ColdTransitionLst } )
when is_atom( Place ),
     is_atom( Mod ) ->

  UserState1 = apply( Mod, place_add, [Place, Token, UserState] ),
  UserState2 = eval_pnet( Mod, UserState1 ),
  UserState3 = eval_cold( Mod, UserState2, ColdTransitionLst ),

  {noreply, ModState#mod_state{ user_state = UserState3 }}.



init( Mod ) ->

  UserState1 = apply( Mod, init, [] ),
  UserState2 = eval_pnet( Mod, UserState1 ),

  {ok, #mod_state{ mod = Mod, user_state = UserState2}}.


%%====================================================================
%% API functions
%%====================================================================

-spec start_link( Mod :: atom() ) -> {ok, pid()}.

start_link( Mod ) when is_atom( Mod ) ->
  gen_server:start_link( ?MODULE, Mod, [] ).


%%====================================================================
%% Internal functions
%%====================================================================

-spec eval_pnet( Mod, UserState ) -> _
when Mod       :: atom(),
     UserState :: _.

eval_pnet( Mod, UserState )
when is_atom( Mod ) ->

  % fetch list of hot transitions
  HotTransitionLst = [T || {_,T} <- lists:sort( [ {rand:uniform(), T} || T <- apply( Mod, hot_transition_lst, [] )] )],

  % step through all hot transitions
  F = fun( T, #step_result{ has_changed = Hc, user_state = Us } ) ->
        R = step_hot_transition( T, Mod, Us ),
        #step_result{ has_changed = Hc1 } = R,
        R#step_result{ has_changed = Hc orelse Hc1 }
      end,

  R = lists:foldl( F, #step_result{ user_state = UserState }, HotTransitionLst ),
  #step_result{ has_changed = HasChanged, user_state = UserState1 } = R,

  case HasChanged of

    % if no transitions were enabled then return
    false -> UserState1;

    % if any of the transition were enabled then continue evaluation
    true -> eval_pnet( Mod, UserState1 )
  
  end.

-spec step_hot_transition( T, Mod, UserState ) -> #step_result{}
when T         :: atom(),
     Mod       :: atom(),
     UserState :: _.

step_hot_transition( T, Mod, UserState )
when is_atom( T ),
     is_atom( Mod ) ->

  ConsumeMapLst = apply( Mod, consume_map_lst, [T, UserState] ),

  case ConsumeMapLst of

    []    -> #step_result{ user_state = UserState };
    [_|_] ->

      % pick random consume map
      I = rand:uniform( length( ConsumeMapLst ) ),
      Cm = lists:nth( I, ConsumeMapLst ),
      
      Pm = apply( Mod, produce_map, [T, Cm] ),


      UserState1 = apply_place_op( place_remove, Cm, Mod, UserState ),
      UserState2 = apply_place_op( place_add, Pm, Mod, UserState1 ),

      #step_result{ has_changed = true, user_state = UserState2 }
  end.

-spec apply_place_op( Op, Map, Mod, UserState ) -> _
when Op        :: place_remove | place_add,
     Map       :: #{atom() => [_]},
     Mod       :: atom(),
     UserState :: _.

apply_place_op( Op, Map, Mod, UserState )
when Op =:= place_remove orelse Op =:= place_add,
     is_map( Map ),
     is_atom( Mod ) ->

  F = fun( Place, Us ) ->
        #{ Place := TokenLst } = Map,
        apply( Mod, Op, [Place, TokenLst, Us] )
      end,

  lists:foldl( F, UserState, maps:keys( Map ) ).

-spec eval_cold( Mod, UserState, ColdTransitionLst ) -> _
when Mod :: atom(),
     UserState :: _,
     ColdTransitionLst :: [#cold_transition{}].

eval_cold( Mod, UserState, ColdTransitionLst ) ->

  F = fun( #cold_transition{ src_place = SrcPlace,
                             pnet_pid = PnetPid,
                             dest_place = DestPlace }, Us ) ->

        % fetch tokens on source state
        TokenLst = apply( Mod, place_get, [SrcPlace, Us] ),

        % send tokens to remote Petri net
        lists:foreach( fun( T ) ->
                         PnetPid ! #add_token{ place = DestPlace, token = T }
                       end,
                       TokenLst ),

        % remove tokens on source state
        Cm = #{ SrcPlace => TokenLst },
        apply_place_op( place_remove, Cm, Mod, Us )

      end,

  lists:foldl( F, UserState, ColdTransitionLst ).



