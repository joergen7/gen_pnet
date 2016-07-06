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

-module( trsn_handler ).

-behaviour( gen_event ).

-export( [init/1, handle_call/2, handle_info/2, terminate/2, handle_event/2,
          code_change/3] ).

-include( "gen_pnet.hrl" ).

%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, user_info, pnet_pid, trsn } ).

%%====================================================================
%% gen_server callback functions
%%====================================================================

-spec init( {Mod, UserInfo, PnetPid, Trsn} ) -> {ok, State}
when Mod      :: atom(),
     UserInfo :: _,
     PnetPid  :: pid(),
     Trsn     :: atom(),
     State    :: #mod_state{}.

init( {Mod, UserInfo, PnetPid, Trsn} )
when is_atom( Mod ),
     is_pid( PnetPid ),
     is_atom( Trsn ) ->

  io:format( "trsn_handler:init() {~p, ~p, ~p, ~p} )~n",
             [Mod, UserInfo, PnetPid, Trsn] ),

  State = #mod_state{ mod       = Mod,
                      user_info = UserInfo,
                      pnet_pid  = PnetPid,
                      trsn      = Trsn },

  {ok, State}.

terminate( _Arg, _State ) ->
  ok.

handle_call( Request, State = #mod_state{ mod = Mod, trsn = Trsn } ) ->

  error_logger:warning_report( [{module, trsn_handler}, {callback, handle_call},
                                {mod, Mod}, {trsn, Trsn}, {request, Request},
                                {return, {error, unsupported_op}}] ),

  {ok, {error, unsupported_op}, State}.


handle_info( {'EXIT', _Pid, normal}, State ) -> {ok, State};

handle_info( Info, State = #mod_state{ mod = Mod, trsn = Trsn } ) ->

  error_logger:warning_report( [{module, trsn_handler}, {callback, handle_info},
                                {mod, Mod}, {trsn, Trsn}, {info, Info},
                                {action, ignored}] ),

  {ok, State}.

code_change( _OldVsn, State = #mod_state{}, _Extra ) ->
  {ok, State}.

handle_event( place_update,
              State = #mod_state{ mod       = Mod,
                                  user_info = UserInfo,
                                  pnet_pid  = PnetPid,
                                  trsn      = Trsn } ) ->

  io:format( "trsn_handler:handle_event( place_update, #mod_state{ trsn = ~p } )~n", [Trsn] ),

  % query token map showing what tokens are on what place
  TokenMap = token_map( Mod, PnetPid, Trsn ),

  % get all possibilities for this transition to consume tokens
  Ecl = Mod:enum_consum_lst( Trsn, TokenMap, UserInfo ),

  % is there any combination of tokens we can consume?
  case Ecl of

    % if no combination is possible we're done
    [] -> {ok, State};

    % if there are possible combinations
    [_|_] ->

      % pick one combination at random
      ConsumeLst = pick_from( Ecl ),

      % attempt to get a lock on the tokens we want to consume
      case gen_pnet:consume( PnetPid, ConsumeLst ) of

        % if the lock is unavailable we have to try again
        {error, stale_request} -> handle_event( place_update, State );

        % if we managed to get a lock
        ok ->

          F = fun() ->

            io:format( "Firing transition as ~p~n", [self()] ),

            % fire the transition
            ProduceLst = Mod:fire( Trsn, ConsumeLst, UserInfo ),

            % now write the produced tokens back
            gen_pnet:produce( PnetPid, ProduceLst )

          end,

          % fire transition in an extra process
          _ChildPid = spawn_link( F ),

          % meanwhile, attempt to continue firing this transition
          handle_event( place_update, State )
      end
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec pick_from( Lst::[_] ) -> Item::_.

pick_from( Lst ) when is_list( Lst ) ->
  lists:nth( rand:uniform( length( Lst ) ), Lst ).


-spec token_map( Mod::atom(), PnetPid::pid(), Trsn::atom() ) ->
  #{ atom() => [#token{}] }.

token_map( Mod, PnetPid, Trsn )
when is_atom( Mod ),
     is_pid( PnetPid ),
     is_atom( Trsn ) ->


  F = fun( Place, Acc ) ->
        {ok, Lst} = gen_pnet:ls( PnetPid, Place ),
        Acc#{ Place => Lst }
      end,

  lists:foldl( F, #{}, Mod:preset( Trsn ) ).
