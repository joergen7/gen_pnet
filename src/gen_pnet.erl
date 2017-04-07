%% -*- erlang -*-
%%
%% A generic Petri net OTP library.
%%
%% Copyright 2016 Jorgen Brandt. All Rights Reserved.
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
%% @author Jorgen Brandt <joergen.brandt@onlinehome.de>

-module( gen_pnet ).

-behaviour( gen_server ).

-export( [start_link/1, start_link/2, ls/2, call/2, cast/2, produce/2,
          produce_token_lst/3, produce_token/3] ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          init/1, terminate/2] ).

-include( "gen_pnet.hrl" ).

%%====================================================================
%% Callback definitions
%%====================================================================

-callback handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
            {reply, _} | {reply, _, #{ atom() => [_] }}.

-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
            noreply | {noreply, #{ atom() => [_] }}.

-callback trigger_map() ->
            #{ atom() => fun( ( _ ) -> ok )}.

-callback place_lst() ->
            [atom()].

-callback trsn_lst() ->
            [atom()].

-callback init_marking() ->
            #{ atom() => [_] }.

-callback preset( atom() ) ->
            [atom()].

-callback is_enabled( atom(), #{ atom() => [_]} ) ->
            boolean().

-callback fire( atom(), #{ atom() => [_] } ) ->
            pass | {produce, #{ atom() => [_] }}.

%%====================================================================
%% API functions
%%====================================================================

start_link( Mod ) when is_atom( Mod ) ->
  start_link( #net_state{ iface_mod = Mod, net_mod = Mod } );

start_link( NetState = #net_state{} ) ->
  gen_server:start_link( ?MODULE, NetState, [] ).

start_link( ServerName, Mod ) when is_atom( Mod ) ->
  start_link( ServerName, #net_state{ iface_mod = Mod, net_mod = Mod } );

start_link( ServerName, NetState = #net_state{} ) ->
  gen_server:start_link( ServerName, ?MODULE, NetState, [] ).

ls( Pid, Place ) ->
  gen_server:call( Pid, {ls, Place} ).

call( Pid, Request ) ->
  gen_server:call( Pid, {call, Request} ).

cast( Pid, Request ) ->
  gen_server:cast( Pid, {cast, Request} ).

produce( Pid, ProdMap ) ->
  gen_server:cast( Pid, {produce, ProdMap} ).

produce_token_lst( Pid, Place, TokenLst ) ->
  produce( Pid, #{ Place => TokenLst } ).

produce_token( Pid, Place, Token ) ->
  produce( Pid, #{ Place => [Token] } ).


%%====================================================================
%% Generic server callback functions
%%====================================================================

code_change( _OldVsn, State, _Extra ) -> {ok, State}.


handle_call( {ls, Place}, _From,
             NetState = #net_state{ marking = Marking } ) ->

  Reply = case maps:is_key( Place, Marking ) of
            true  -> {ok, maps:get( Place, Marking )};
            false -> {error, #bad_place{ name = Place }}
          end,

  {reply, Reply, NetState};

handle_call( {call, Request}, From,
             NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_call( Request, From, NetState ) of

    {reply, Reply} ->
      {reply, Reply, NetState};
    
    {reply, Reply, ProdMap} ->
      produce( self(), ProdMap ),
      {reply, Reply, NetState}

  end.

handle_cast( {produce, ProdMap}, NetState = #net_state{} ) ->
  % TODO
  {noreply, NetState};

handle_cast( {cast, Request}, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_cast( Request, NetState ) of

    noreply ->
      {noreply, NetState};

    {noreply, ProdMap} ->
      produce( self(), ProdMap ),
      {noreply, NetState}

  end.


handle_info( _Info, State ) ->
  {noreply, State}.


init( NetState = #net_state{ marking = ArgInitMarking, net_mod = NetMod } ) ->

  PlaceLst = NetMod:place_lst(),
  ModInitMarking = NetMod:init_marking(),

  F = fun( P, Acc ) ->
        ModTkLst = maps:get( P, ModInitMarking, [] ),
        ArgTkLst = maps:get( P, ArgInitMarking, [] ),
        Acc#{ P => ModTkLst++ArgTkLst }
      end,

  Marking = lists:fold( F, #{}, PlaceLst ),

  {ok, NetState#net_state{ marking = Marking }}.


terminate( _Reason, _State ) ->
  ok.


%%====================================================================
%% Internal functions
%%====================================================================

-spec pick_mod( #{ atom() => [_] }, atom() ) ->
        pass | {produce, #{ atom() => [_] }}.

pick_mod( Marking, NetMod ) ->

  % get all transitions in the net
  TrsnLst = NetMod:trsn_lst(),

  F = fun( T, Acc ) ->
        Preset = NetMod:preset( T ),
        MLst = enum_mod( Preset, Marking ),
        IsEnabled = fun( M ) -> NetMod:is_enabled( T, M ) end,
        EnabledMLst = lists:filter( IsEnabled, MLst ),
        case EnabledMLst of
          []    -> Acc;
          [_|_] -> Acc#{ T => EnabledMLst }
        end
      end,

  % derive a map listing all enabled modes for each transition
  ModMap = lists:fold( F, #{}, TrsnLst ),

  try

    % pick an enabled transition or terminate if no transition is enabled
    Trsn = case maps:keys( ModMap ) of
             []     -> throw( pass );
             KeyLst -> lib_combin:pick( KeyLst )
           end,

    % get the list of modes for which the chosen transition is enabled
    #{ Trsn := ModLst } = ModMap,

    % pick a mode for the chosen transition
    Mod = lib_combin:pick( ModLst ),

    {ok, Mod}

  catch
    throw:pass -> pass
  end.


-spec enum_mod( [atom()], #{ atom() => [_] } ) -> [#{ atom() => [_] }].

enum_mod( Preset, Marking ) ->

  F = fun( P, Acc ) ->
        N = maps:get( P, Acc, 0 ),
        Acc#{ P => N+1 }
      end,

  % gather count map
  CountMap = lists:foldl( F, #{}, Preset ),

  G = fun( P, N, Acc ) ->
        #{ P := TkLst } = Marking,
        Acc#{ P => lib_combin:cnr( N, TkLst ) }
      end,

  % enumerate drawing combinations for each preset place individually
  CmbMap = maps:fold( G, #{}, CountMap ),

  % enumerate permutations of map containing drawing combinations
  lib_combin:permut_map( CmbMap ).