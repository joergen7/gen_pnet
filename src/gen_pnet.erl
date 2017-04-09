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

-export( [start_link/1, start_link/2, ls/2, marking/1, call/2, cast/2,
          produce/2, produce_token_lst/3, produce_token/3, stats/1] ).

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

marking( Pid ) ->
  gen_server:call( Pid, marking ).

stats( Pid ) ->
  gen_server:call( Pid, stats ).

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


handle_call( {ls, Place}, _From, NetState = #net_state{ marking = Marking } ) ->

  Reply = case maps:is_key( Place, Marking ) of
            true  -> {ok, maps:get( Place, Marking )};
            false -> {error, #bad_place{ name = Place }}
          end,

  {reply, Reply, NetState};

handle_call( marking, _From, NetState = #net_state{ marking = Marking } ) ->
  {reply, Marking, NetState};

handle_call( {call, Request}, From,
             NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_call( Request, From, NetState ) of

    {reply, Reply} ->
      {reply, Reply, NetState};
    
    {reply, Reply, ProdMap} ->
      produce( self(), ProdMap ),
      {reply, Reply, NetState}

  end;

handle_call( stats, _From, NetState = #net_state{ stats = Stats } ) ->
  {reply, Stats, NetState}.

handle_cast( {produce, ProdMap},
             NetState = #net_state{ stats  = Stats,
                                    tstart = T1,
                                    cnt    = Cnt } ) ->
  
  NetState1 = prd( NetState, ProdMap ),
  NetState2 = handle_trigger( NetState1 ),

  case progress( NetState2 ) of

    pass ->
      {noreply, NetState2};

    {delta, Mode, Pm} ->

      NetState3 = cns( NetState2, Mode ),
      produce( self(), Pm ),

      NetState4 = if
                    Cnt < 1000 -> NetState3#net_state{ cnt = Cnt+1 };
                    true     ->

                      T2 = os:system_time(),
                      Tmean = round( ( T1+T2 )/2 ),
                      Tdelta = T2-T1,
                      CurrentFps = 1000000000000/Tdelta,

                      Current = #stat{ t = Tmean, fps = CurrentFps },

                      {Hi1, Lo1} = case Stats of
                                     undefined -> {Current, Current};
                                     #stats{ hi = H, lo = L } -> {H, L}
                                   end,

                      #stat{ fps = HiFps } = Hi1,
                      #stat{ fps = LoFps } = Lo1,

                      Hi2 = if
                              CurrentFps > HiFps -> Current;
                              true               -> Hi1
                            end,

                      Lo2 = if
                              CurrentFps < LoFps -> Current;
                              true               -> Lo1
                            end,

                      NetState3#net_state{ stats  = #stats{ current = Current,
                                                            hi      = Hi2,
                                                            lo      = Lo2 },
                                           tstart = T2,
                                           cnt    = 0 }
                  end,

      {noreply, NetState4}

  end;

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

  F = fun( P, Acc ) ->
        ArgTkLst = maps:get( P, ArgInitMarking, [] ),
        Acc#{ P => ArgTkLst }
      end,

  Marking = lists:foldl( F, #{}, PlaceLst ),

  produce( self(), NetMod:init_marking() ),

  {ok, NetState#net_state{ marking = Marking,
                           stats   = undefined,
                           tstart  = os:system_time(),
                           cnt     = 0 }}.


terminate( _Reason, _State ) ->
  ok.


%%====================================================================
%% Internal functions
%%====================================================================

-spec handle_trigger( #net_state{} ) -> #net_state{}.

handle_trigger( NetState = #net_state{ marking   = Marking,
                                       iface_mod = IfaceMod } ) ->

  TriggerMap = IfaceMod:trigger_map(),

  F = fun( P, TkLst, Acc ) ->
        case maps:is_key( P, TriggerMap ) of
          false -> Acc#{ P => TkLst };
          true  ->
            lists:foreach( maps:get( P, TriggerMap ), TkLst ),
            Acc#{ P => [] }
        end
      end,

  Marking1 = maps:fold( F, #{}, Marking ),

  NetState#net_state{ marking = Marking1 }.


-spec cns( #net_state{}, #{ atom() => [_] } ) -> _.

cns( NetState = #net_state{ marking = Marking }, Mode ) ->

  F = fun( T, TkLst, Acc ) ->
        #{ T := TkLst } = Marking,
        Acc#{ T => TkLst--maps:get( T, Mode, [] ) }
      end,

  NetState#net_state{ marking = maps:fold( F, #{}, Marking ) }.

-spec prd( #net_state{}, _ ) -> _.

prd( NetState = #net_state{ marking = Marking }, ProdMap ) ->

  F = fun( T, TkLst, Acc ) ->
        #{ T := TkLst } = Marking,
        Acc#{ T => TkLst++maps:get( T, ProdMap, [] ) }
      end,

  NetState#net_state{ marking = maps:fold( F, #{}, Marking ) }.

-spec progress( #net_state{} ) ->
        pass | {delta, #{ atom() => [_]}, #{ atom() => [_] }}.

progress( #net_state{ marking = Marking, net_mod = NetMod } ) ->

  % get all transitions in the net
  TrsnLst = NetMod:trsn_lst(),

  F = fun( T, Acc ) ->
        Preset = NetMod:preset( T ),
        MLst = enum_mode( Preset, Marking ),
        IsEnabled = fun( M ) -> NetMod:is_enabled( T, M ) end,
        EnabledMLst = lists:filter( IsEnabled, MLst ),
        case EnabledMLst of
          []    -> Acc;
          [_|_] -> Acc#{ T => EnabledMLst }
        end
      end,

  % derive a map listing all enabled modes for each transition
  ModeMap = lists:foldl( F, #{}, TrsnLst ),

  % delegate enabled mode map to attempt_progress function
  attempt_progress( ModeMap, NetMod ).


-spec attempt_progress( map(), atom() ) -> pass | {delta, _, _}.

attempt_progress( ModeMap, NetMod ) ->

  case maps:size( ModeMap ) of

    0 -> pass;
    _ ->

      TrsnLst = maps:keys( ModeMap ),
      Trsn = lib_combin:pick_from( TrsnLst ),
      #{ Trsn := ModeLst } = ModeMap,
      Mode = lib_combin:pick_from( ModeLst ),

      case NetMod:fire( Trsn, Mode ) of

        {produce, ProdMap} ->
          {delta, Mode, ProdMap};
        
        pass ->
          attempt_progress( ModeMap#{ Trsn := ModeLst--Mode }, NetMod )

      end
  end.


-spec enum_mode( [_], _ ) -> _.

enum_mode( Preset, Marking ) ->

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