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


%%--------------------------------------------------------------------
%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%% @doc A module representing a Petri net.
%%
%% To define the structure of a Petri net, users implement a number of
%% callback functions laid out in the gen_pnet module.
%%
%% <h3>User module exports</h3>
%%
%% <h4>init/1</h4>
%% An initialization function called when the gen_pnet module is started.
%% Returns
%% <ul>
%%   <li>the initial marking of the Petri net in the form of a token list</li>
%%   <li>a user-defined data structure that is made available when enumerating
%%       possible consumption lists with `enum_consume_lst/1' and when a
%%       transition is fired with `fire/2'. Note that this user-defined data
%%       structure cannot be updated afterwards.</li>
%% </ul>
%% ```
%%  init( InitArg :: _ )  
%%    -> {ok, [#token{}], _}
%% '''
%% <h4>place_lst/0</h4>
%% The `place_lst' function returns a list of atoms denoting the names of the
%% places in the Petri net's structure.
%% ```
%%  place_lst()  
%%    -> [atom()]
%% '''
%% <h4>trsn_lst/0</h4>
%% The `trsn_lst' function returns a list of atoms denoting the names of the
%% transitions in the Petri net's structure.
%% ```
%%  trsn_lst()  
%%    -> [atom()]
%% '''
%% <h4>preset/1</h4>
%% The `preset' function returns the preset of a given transition. I.e., it
%% enumerates all places, this transition may consume tokens from.
%% ```
%%  preset( Trsn::atom() )  
%%    -> [atom()]
%% '''
%% <h4>enum_consume_lst/3</h4>
%% The `enum_consume_lst' function consumes a map associating a place atom with
%% a list of tokens and returns a list of lists, where each list contains a
%% combination of tokens that can be consumed firing a given transition. Returns
%% the empty list if the transition is not enabled. One of the enumerated lists
%% is chosen to be handed to the `fire/2' function firing the transition.
%% ```
%%  enum_consume_lst( Trsn     :: atom(),
%%                    TokenMap :: #{ atom() => [#token{}]},
%%                    UserInfo :: _ )
%%    -> [[#token{}]]
%% '''
%%
%% <h4>fire/3</h4>
%% Called to fire a transition with a given consumption list. `fire/2' must
%% return the list of tokens that is produced.
%% ```
%%  fire( Trsn       :: atom(),
%%        ConsumeLst :: [#token{}],
%%        UserInfo   :: _ )
%%    -> [#token{}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------

-module( gen_pnet ).

-behaviour( gen_server ).

-export( [start_link/3, start_link/4, stop/1, ls/2, which_trsns/2, consume/2,
          produce/2, add/2] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3] ).

-include( "gen_pnet.hrl" ).

%%====================================================================
%% Callback function definition
%%====================================================================

-callback init( InitArg::_ ) ->
  {ok, InitMarking::[#token{}], UserInfo::_}.

-callback place_lst()
  -> PlaceLst::[atom()].

-callback trsn_lst()
  -> TrsnLst::[atom()].

-callback preset( Trsn::atom() )
  -> PlaceLst::[atom()].

-callback enum_consume_lst( Trsn     :: atom(),
                            TokenMap :: #{ atom() => [#token{}] },
                            UserInfo :: _)
  -> [[#token{}]].

-callback fire( Trsn       :: atom(),
                ConsumeLst :: [#token{}],
                UserInfo   :: _ )
  -> ProduceLst::[#token{}].


%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, user_info, mgr_map, token_lst = [] } ).


%%====================================================================
%% API functions
%%====================================================================

-spec start_link( Mod, UserArg, Options ) -> Result
when Mod     :: atom(),
     UserArg    :: _,
     Options :: [tuple()],
     Result  :: _.

start_link( Mod, UserArg, Options )
when is_atom( Mod ),
     is_list( Options ) ->

  gen_server:start_link( ?MODULE, {Mod, UserArg}, Options ).

-spec start_link( ServerName, Mod, UserArg, Options ) -> Result
when ServerName :: tuple(),
     Mod        :: atom(),
     UserArg       :: _,
     Options    :: [tuple()],
     Result     :: _.

start_link( ServerName, Mod, UserArg, Options )
when is_tuple( ServerName ),
     is_atom( Mod ),
     is_list( Options ) ->

  start_link( ServerName, ?MODULE, {Mod, UserArg}, Options ).

-spec stop( ServerRef::_ ) -> stopped.

stop( ServerRef ) ->
  gen_server:call( ServerRef, stop ).

-spec ls( ServerRef::_, Place::atom() ) ->
  {ok, [#token{}]} | {error, no_such_place}.

ls( ServerRef, Place ) when is_atom( Place ) ->
  gen_server:call( ServerRef, {ls, Place} ).

-spec consume( ServerRef::_, ConsumeLst::[#token{}] ) ->
  ok | {error, stale_request}.

consume( ServerRef, ConsumeLst ) when is_list( ConsumeLst ) ->
  gen_server:call( ServerRef, {consume, ConsumeLst} ).

-spec produce( ServerRef::_, ProduceLst::[#token{}] ) -> ok.

produce( ServerRef, ProduceLst ) when is_list( ProduceLst ) ->
  gen_server:call( ServerRef, {produce, ProduceLst} ).

-spec which_trsns( Mod::atom(), Place::atom() ) -> [atom()].

which_trsns( Mod, Place ) when is_atom( Place ) ->
  
  F = fun( Trsn, Acc ) ->
        case lists:member( Place, Mod:preset( Trsn ) ) of
          false -> Acc;
          true  -> [Trsn|Acc]
        end
      end,

  lists:foldl( F, [], Mod:trsn_lst() ).

  -spec add( ServerRef::_, Token::#token{} ) -> ok.

  add( ServerRef, Token = #token{} ) ->
    gen_server:call( ServerRef, {add, Token} ).

%%====================================================================
%% gen_server callback functions
%%====================================================================

-type init_arg() :: {Mod::atom(), UserArg::_}.

-spec init( InitArg::init_arg() ) -> {ok, State::#mod_state{}}.

init( {Mod, UserArg} ) when is_atom( Mod ) ->

  io:format( "gen_pnet:init() {~p, ~p} )~n", [Mod, UserArg] ),

  {ok, TokenLst, UserInfo} = Mod:init( UserArg ),

  F = fun( Place, Acc ) ->

        {ok, Pid} = gen_event:start_link(),

        G = fun( Trsn ) ->
              gen_event:add_handler( Pid, trsn_handler, {Mod, UserInfo, self(), Trsn} )
            end,

        ok = lists:foreach( G, which_trsns( Mod, Place ) ),

        Acc#{ Place => Pid }

      end,


  MgrMap = lists:foldl( F, #{}, Mod:place_lst() ),


  % create pnet state
  State    = #mod_state{ mod       = Mod,
                         user_info = UserInfo,
                         mgr_map   = MgrMap,
                         token_lst = TokenLst },

  {ok, State}.

-type call_reply()   :: ok
                      | {ok, [#token{}]}
                      | {error, stale_request
                              | no_such_place
                              | unsupported_op}.

-type call_return()  :: {stop, normal, stopped, #mod_state{}}
                      | {reply, call_reply(), #mod_state{}}.

-spec handle_call( Request, {Tag, Pid}, State ) -> Result
when Request  :: _,
     Tag      :: _,
     Pid      :: pid(),
     State    :: #mod_state{},
     Result   :: call_return().

handle_call( stop, _From, State = #mod_state{} ) ->
  {stop, normal, stopped, State};

handle_call( {ls, Place}, _From,
             State = #mod_state{ mod = Mod, token_lst = TokenLst } )
when is_atom( Place ) ->

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      {reply, {error, no_such_place}, State};

    true  ->
      Result = [X || X = #token{ place = P } <- TokenLst, P =:= Place],
      {reply, {ok, Result}, State}

  end;

handle_call( {consume, ConsumeLst}, _From,
             State = #mod_state{ token_lst = TokenLst } )
when is_list( ConsumeLst ) ->

  io:format( "gen_pnet:handle_call( {consume, ~p}, _, ~p )~n", [ConsumeLst, State] ),

  TokenLst1 = TokenLst--ConsumeLst,
  L = length( TokenLst )-length( ConsumeLst ),
  case length( TokenLst1 ) of

    L ->

      State1 = State#mod_state{ token_lst = TokenLst1 },

      io:format( "  success --> ~p~n", [State1] ),

      {reply, ok, State1};

    _ ->

      io:format( "  error: stale request" ),

      {reply, {error, stale_request}, State}

  end;

handle_call( {add, Token = #token{ place = Place }}, _From,
             State = #mod_state{ mod = Mod, mgr_map = MgrMap, token_lst = TokenLst } )
when is_atom( Place ),
     is_atom( Mod ),
     is_map( MgrMap ),
     is_list( TokenLst ) ->

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      {reply, {error, no_such_place}, State};

    true ->

      #{ Place := Mgr } = MgrMap,
      io:format( "notifying mgr ...~n" ),
      gen_event:notify( Mgr, place_update ),

      {reply, ok, State#mod_state{ token_lst = [Token|TokenLst] }}

  end;

handle_call( {produce, AddLst}, _From,
             State = #mod_state{ mod       = Mod,
                                 mgr_map   = MgrMap,
                                 token_lst = TokenLst } )
when is_list( AddLst ) ->

  PlaceLst = Mod:place_lst(),

  F = fun( Token = #token{ place = Place }, Acc ) ->
        case lists:member( Place, PlaceLst ) of
          false -> error( {no_such_place, Place} );
          true  -> [Token|Acc]
        end
      end,

  TokenLst1 = lists:foldl( F, TokenLst, AddLst ),

  NotifyLst = lists:usort( [P || #token{ place = P } <- AddLst] ),

  G = fun( P ) ->
        #{ P := Mgr } = MgrMap,
        gen_event:notify( Mgr, place_update )
      end,

  ok = lists:foreach( G, NotifyLst ),

  {reply, ok, State#mod_state{ token_lst = TokenLst1 }};

handle_call( Request, _From, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_call},
                                {mod, Mod}, {request, Request},
                                {return, {error, unsupported_op}}] ),

  {reply, {error, unsupported_op}, State}.



-spec handle_cast( Request, State ) -> {noreply, NewState}
when Request  :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_cast( Request, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_cast},
                                {mod, Mod}, {request, Request},
                                {action, ignored}] ),


  {noreply, State}.

-spec handle_info( Info, State ) -> {noreply, NewState}
when Info     :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_info( Info, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_info},
                                {mod, Mod}, {info, Info},
                                {action, ignored}] ),

  {noreply, State}.



-spec terminate( Reason::_, State::#mod_state{} ) -> ok.

terminate( Reason, _State ) ->

  io:format( "gen_pnet:terminate( ~p, _ )~n", [Reason] ),

  ok.

-spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
when OldVsn   :: string(),
     State    :: #mod_state{},
     Extra    :: _,
     NewState :: #mod_state{}.

code_change( _OldVsn, State = #mod_state{}, _Extra ) ->
  {ok, State}.