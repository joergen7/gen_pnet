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

-export( [new/3, start_link/2, start_link/3, ls/2, marking/1, call/2, cast/2,
          produce/2, produce_token_lst/3, produce_token/3, get_stats/1,
          reset_stats/1, stop/1] ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          init/1, terminate/2] ).

-include( "gen_pnet.hrl" ).

%%====================================================================
%% Callback definitions
%%====================================================================

%% Interface callbacks

-callback code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
            {ok, #net_state{}} | {error, _}.

-callback handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
            {reply, _} | {reply, _, #{ atom() => [_] }}.

-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
            noreply | {noreply, #{ atom() => [_] }}.

-callback handle_info( Info :: _, NetState :: #net_state{} ) ->
            noreply | {noreply, #{ atom() => [_] }}.

-callback terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

-callback trigger( Place :: atom(), Token :: _ ) -> pass | consume.


%% Net callbacks

-callback place_lst() -> [atom()].

-callback trsn_lst() -> [atom()].

-callback init_marking() -> #{ atom() => [_] }.

-callback preset( Place :: atom() ) -> [atom()].

-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]} ) -> boolean().

-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] } ) ->
            pass | {produce, #{ atom() => [_] }}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Generates an initial instance of a state record.
%%
%%      Such a state record can be used to initialize a `gen_pnet' instance with
%%      `start_link/1' or `start_link/2'.
%%
%% @see start_link/2
%% @see start_link/3
new( InitMarking, NetMod, IfaceMod ) ->
  #net_state{ marking = InitMarking, net_mod = NetMod, iface_mod = IfaceMod }.

%% @doc Starts an unregistered net instance.
%%
%%      The `gen_pnet' instance can be initialized with either the callback
%%      module name `Mod', implementing all callback functions or with a
%%      `#net_state{}' record instance. Such a `#net_state{}' record can be
%%      generated using the `new/3' function. The option list `Options' is
%%      handed down to `gen_server:start_link/3' as is.
%%
%% @see new/3
start_link( Mod, Options ) when is_atom( Mod ) ->
  start_link( #net_state{ iface_mod = Mod, net_mod = Mod }, Options );

start_link( NetState = #net_state{}, Options ) ->
  gen_server:start_link( ?MODULE, NetState, Options ).

%% @doc Starts a net instance registered to `ServerName' using the callback
%%      module `Mod' or a `#net_state' record instance which can be created
%%      using `new/3'. Herein, the `ServerName' argument can be
%%      `{local, Name} | {global, Name} | {via, Module, ViaName}'. The server
%%      name `ServerName' and option list `Options' are handed down to
%%      `gen_server:start_link/4' as is.
%%
%% @see new/3
start_link( ServerName, Mod, Options ) when is_atom( Mod ) ->
  NetState = #net_state{ iface_mod = Mod, net_mod = Mod },
  start_link( ServerName, NetState, Options );

start_link( ServerName, NetState = #net_state{}, Options ) ->
  gen_server:start_link( ServerName, ?MODULE, NetState, Options ).

%% @doc Requests the net instance under process id `Pid' to list all
%%      tokens on the place named `Place'.
%%
%%      Herein, `Pid' can also be a registered process name. The return value is
%%      either `{ok, [_]}'' if the place exists or a `{error, #bad_place{}}'
%%      tuple.
ls( Pid, Place ) ->
  gen_server:call( Pid, {ls, Place} ).

%% @doc Requests the net instance under process id `Pid' to return a
%%      marking map, associating to each place name the list of tokens that this
%%      place holds.
%%
%%      Herein, `Pid' can also be a registered process name. The return value is
%%      just the plain map.
marking( Pid ) ->
  gen_server:call( Pid, marking ).

%% @doc Requests the net instance under process id `Pid' to return the
%%      throughput of the net.
%%
%%      The throughput is given as a `#stats{}' record consisting of three
%%      `#stat{}' record instances characterizing the current, maximum, and
%%      minimum throughput of this net in transition firings per second.
get_stats( Pid ) ->
  gen_server:call( Pid, get_stats ).

%% @doc Requests the net instance under process id `Pid' to clear its stats.
reset_stats( Pid ) ->
  gen_server:call( Pid, reset_stats ).

%% @doc Signal the net instance under process id `Pid' to stop.
stop( Pid ) ->
  gen_server:stop( Pid ).

%% @doc Send the request term `Request' to the net instance under process id
%%      `Pid' and return the reply.
%%
%%      The request is handled by the `handle_call/3' callback function of the
%%      interface module.
call( Pid, Request ) ->
  gen_server:call( Pid, {call, Request} ).

%% @doc Send the request term `Request' asynchronously to the net instance under
%%      process id `Pid'.
%%
%%      The request is handled by the `handle_cast/2' callback function of the
%%      interface module. Note that the cast succeeds even if a non-existing
%%      process is addressed or the net instance is down.
cast( Pid, Request ) ->
  gen_server:cast( Pid, {cast, Request} ).

%% @doc Produce the tokens on the places as described in the `ProdMap' argument
%%      atomically in the net instance under process id `Pid'.
%%
%%      Note that production succeeds even if a non-existing process is
%%      addressed or the net instance is down.
produce( Pid, ProdMap ) ->
  gen_server:cast( Pid, {produce, ProdMap} ).

%% @doc Produce the tokens in `TokenLst' all on place `Place' atomically in the
%%      net instance under process id `Pid'.
produce_token_lst( Pid, Place, TokenLst ) ->
  produce( Pid, #{ Place => TokenLst } ).

%% @doc Produce the single token `Token' on place `Place' in the net instance
%%      under process id `Pid'.
produce_token( Pid, Place, Token ) ->
  produce( Pid, #{ Place => [Token] } ).


%%====================================================================
%% Generic server callback functions
%%====================================================================

%% @private
code_change( OldVsn, NetState = #net_state{ iface_mod = IfaceMod }, Extra ) ->
  IfaceMod:code_change( OldVsn, NetState, Extra ).


%% @private
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

handle_call( get_stats, _From, NetState = #net_state{ stats = Stats } ) ->
  {reply, Stats, NetState};

handle_call( reset_stats, _From, NetState ) ->
  {reply, ok, NetState#net_state{ stats = undefined }}.


%% @private
handle_cast( {produce, ProdMap},
             NetState = #net_state{ stats  = Stats,
                                    tstart = T1,
                                    cnt    = Cnt } ) ->
  
  NetState1 = handle_trigger( ProdMap, NetState ),

  case progress( NetState1 ) of

    pass ->
      {noreply, NetState1};

    {delta, Mode, Pm} ->

      NetState2 = cns( Mode, NetState1 ),
      produce( self(), Pm ),

      NetState3 = if
                    Cnt < 1000 -> NetState2#net_state{ cnt = Cnt+1 };
                    true       ->

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

                      NetState2#net_state{ stats  = #stats{ current = Current,
                                                            hi      = Hi2,
                                                            lo      = Lo2 },
                                           tstart = T2,
                                           cnt    = 0 }
                  end,

      {noreply, NetState3}

  end;

handle_cast( {cast, Request}, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_cast( Request, NetState ) of

    noreply ->
      {noreply, NetState};

    {noreply, ProdMap} ->
      produce( self(), ProdMap ),
      {noreply, NetState}

  end.


%% @private
handle_info( Info, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_info( Info, NetState ) of

    noreply ->
      {noreply, NetState};

    {noreply, ProdMap} ->
      produce( self(), ProdMap )

  end.


%% @private
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


%% @private
terminate( Reason, NetState = #net_state{ iface_mod = IfaceMod } ) ->
  IfaceMod:terminate( Reason, NetState ).
  


%%====================================================================
%% Internal functions
%%====================================================================

handle_trigger( ProdMap, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  G = fun( P, TkLst, Acc ) ->

        F = fun( Tk, A ) ->
              case IfaceMod:trigger( P, Tk ) of
                pass    -> [Tk|A];
                consume -> A
              end
            end,

        TkLst1 = lists:foldl( F, [], TkLst ),
        Acc#{ P => TkLst1 }

      end,

  ProdMap1 = maps:fold( G, #{}, ProdMap ),
  prd( ProdMap1, NetState ).




-spec cns( #{ atom() => [_] }, #net_state{} ) -> _.

cns( Mode, NetState = #net_state{ marking = Marking } ) ->

  F = fun( T, TkLst, Acc ) ->
        #{ T := TkLst } = Marking,
        Acc#{ T => TkLst--maps:get( T, Mode, [] ) }
      end,

  NetState#net_state{ marking = maps:fold( F, #{}, Marking ) }.

-spec prd( _, #net_state{} ) -> _.

prd( ProdMap, NetState = #net_state{ marking = Marking } ) ->

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