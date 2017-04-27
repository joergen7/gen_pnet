%% -*- erlang -*-
%%
%% A generic Petri net OTP behavior.
%%
%% Copyright 2016-2017 Jorgen Brandt
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
%% -------------------------------------------------------------------
%% @author Jorgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.3
%% @copyright 2016-2017 Jorgen Brandt
%% @see gen_pnet_iface
%% @see gen_pnet_struct
%% @doc Callback function definitions and API for the `gen_pnet' behavior.
%%
%% The callbacks defined for the `gen_pnet' behavior may be separated into a
%% Petri net structure part and an actor interface part. Both behaviors are
%% documented in the `gen_pnet_struct' and `gen_pnet_iface' modules
%% respectively.
%%
%% @end
%% -------------------------------------------------------------------

-module( gen_pnet ).
-behaviour( gen_server ).

%%====================================================================
%% Exports
%%====================================================================

% API functions
-export( [new/2, start_link/3, start_link/4, ls/2, marking/1, call/2, call/3,
          cast/2, get_stats/1, reply/2, reset_stats/1, stop/1, usr_info/1] ).

% gen_server callbacks
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          init/1, terminate/2] ).

% Helper functions
-export( [ls_place/2, get_usr_info/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include( "gen_pnet.hrl" ).

%%====================================================================
%% Type definitions
%%====================================================================

-type name() :: atom()
              | {atom(), atom()}
              | {global, _}
              | {via, atom(), _}
              | pid().

-type server_name() :: {local, atom()}
                     | {global, atom()}
                     | {via, atom(), _}.

-type result() :: {ok, pid()}
                | ignore
                | {error, _}.

%%====================================================================
%% Callback definitions
%%====================================================================

%% Interface callbacks

-callback code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
            {ok, #net_state{}} | {error, _}.

-callback handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.

-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

-callback handle_info( Info :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

-callback init( Args :: _ ) -> {ok, #net_state{}}.

-callback terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

-callback trigger( Place :: atom(), Token :: _, NetState :: #net_state{} ) ->
            pass | drop.



%% Structure callbacks

-callback place_lst() -> [atom()].

-callback trsn_lst() -> [atom()].

-callback init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].

-callback preset( Trsn :: atom() ) -> [atom()].

-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]} ) -> boolean().

-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.

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
-spec new( NetMod :: atom(), UsrInfo :: _ ) -> #net_state{}.

new( NetMod, UsrInfo ) when is_atom( NetMod ) ->
  #net_state{ net_mod = NetMod, usr_info = UsrInfo }.

%% @doc Starts an unregistered net instance.
%%
%%      The `gen_pnet' instance can be initialized with either the callback
%%      module name `Mod', implementing all callback functions or with a
%%      `#net_state{}' record instance. Such a `#net_state{}' record can be
%%      generated using the `new/3' function. The option list `Options' is
%%      handed down to `gen_server:start_link/3' as is.
%%
%% @see new/3
-spec start_link( IfaceMod, Args, Options ) -> result()
when IfaceMod :: atom(),
     Args     :: _,
     Options  :: proplists:proplist().

start_link( IfaceMod, Args, Options )
when is_atom( IfaceMod ), is_list( Options ) ->
  gen_server:start_link( ?MODULE, {IfaceMod, Args}, Options ).

%% @doc Starts a net instance registered to `ServerName' using the callback
%%      module `Mod' or a `#net_state' record instance which can be created
%%      using `new/3'. Herein, the `ServerName' argument can be
%%      `{local, Name} | {global, Name} | {via, Module, ViaName}'. The server
%%      name `ServerName' and option list `Options' are handed down to
%%      `gen_server:start_link/4' as is.
%%
%% @see new/3
-spec start_link( ServerName, IfaceMod, Args, Options ) -> result()
when ServerName :: server_name(),
     IfaceMod   :: atom(),
     Args       :: _,
     Options    :: proplists:proplist().

start_link( ServerName, IfaceMod, Args, Options )
when is_tuple( ServerName ), is_atom( IfaceMod ), is_list( Options ) ->
  gen_server:start_link( ServerName, ?MODULE, {IfaceMod, Args}, Options ).

%% @doc Requests the net instance under process id `Name' to list all
%%      tokens on the place named `Place'.
%%
%%      Herein, `Name' can also be a registered process name. The return value is
%%      either `{ok, [_]}'' if the place exists or a `{error, #bad_place{}}'
%%      tuple.
-spec ls( Name, Place ) -> {ok, [_]} | {error, #bad_place{}}
when Name  :: name(),
     Place :: atom().

ls( Name, Place ) when is_atom( Place ) -> gen_server:call( Name, {ls, Place} ).

%% @doc Requests the net instance under process id `Name' to return a
%%      marking map, associating to each place name the list of tokens that this
%%      place holds.
%%
%%      Herein, `Name' can also be a registered process name. The return value
%%      is the Petri net's marking map.
-spec marking( Name :: name() ) -> #{ atom() => [_] }.

marking( Name ) -> gen_server:call( Name, marking ).


-spec usr_info( Name :: name() ) -> _.

usr_info( Name ) -> gen_server:call( Name, usr_info ).

%% @doc Requests the net instance under process id `Name' to return the
%%      throughput of the net.
%%
%%      The throughput is given as a `#stats{}' record consisting of three
%%      `#stat{}' record instances characterizing the current, maximum, and
%%      minimum throughput of this net in transition firings per second.
-spec get_stats( Name :: name() ) -> #stats{}.

get_stats( Name ) -> gen_server:call( Name, get_stats ).

%% @doc Requests the net instance under process id `Name' to clear its stats.
-spec reset_stats( Name :: name() ) -> ok.

reset_stats( Name ) -> gen_server:call( Name, reset_stats ).

%% @doc Signal the net instance under process id `Name' to stop.
-spec stop( Name :: name() ) -> ok.

stop( Name ) -> gen_server:stop( Name ).

%% @doc Send the request term `Request' to the net instance under process id
%%      `Name' and return the reply.
%%
%%      The request is handled by the `handle_call/3' callback function of the
%%      interface module.
-spec call( Name :: name(), Request :: _ ) -> _.

call( Name, Request ) -> gen_server:call( Name, {call, Request} ).


-spec call( Name :: name(), Request :: _, Timeout :: non_neg_integer() ) -> _.

call( Name, Request, Timeout ) when is_integer( Timeout ), Timeout >= 0 ->
  gen_server:call( Name, {call, Request}, Timeout ).

%% @doc Send the request term `Request' asynchronously to the net instance under
%%      process id `Name'.
%%
%%      The request is handled by the `handle_cast/2' callback function of the
%%      interface module. Note that the cast succeeds even if a non-existing
%%      process is addressed or the net instance is down.
-spec cast( Name :: name(), Request :: _ ) -> ok.

cast( Name, Request ) ->
  gen_server:cast( Name, {cast, Request} ).


-spec reply( Client :: {pid(), _}, Reply :: _ ) -> _.

reply( Client, Reply ) when is_tuple( Client ) ->
  gen_server:reply( Client, Reply ).

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

handle_call( usr_info, _From, NetState = #net_state{ usr_info = UsrInfo } ) ->
  {reply, UsrInfo, NetState};

handle_call( {call, Request}, From,
             NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_call( Request, From, NetState ) of

    {reply, Reply} ->
      {reply, Reply, NetState};
    
    {reply, Reply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      produce( self(), ProdMap ),
      {reply, Reply, NetState1};

    noreply ->
      {noreply, NetState};

    {noreply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      produce( self(), ProdMap ),
      {noreply, NetState1};

    {stop, Reason, Reply} ->
      {stop, Reason, Reply, NetState}

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

    abort ->
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

    {noreply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      produce( self(), ProdMap ),
      {noreply, NetState1};

    {stop, Reason} ->
      {stop, Reason, NetState}

  end.


%% @private
handle_info( Info, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_info( Info, NetState ) of

    noreply ->
      {noreply, NetState};

    {noreply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      produce( self(), ProdMap ),
      {noreply, NetState1};

    {stop, Reason} ->
      {stop, Reason, NetState}

  end.


%% @private
init( {IfaceMod, Args} ) ->

  {ok, NetState} = IfaceMod:init( Args ),
  #net_state{ net_mod = NetMod, usr_info = UsrInfo } = NetState,

  PlaceLst = NetMod:place_lst(),

  F = fun( P, Acc ) ->
        Acc#{ P => NetMod:init_marking( P, UsrInfo ) }
      end,

  InitMarking = lists:foldl( F, #{}, PlaceLst ),

  produce( self(), #{} ),

  {ok, NetState#net_state{ iface_mod = IfaceMod,
                           marking   = InitMarking,
                           stats     = undefined,
                           tstart    = os:system_time(),
                           cnt       = 0 }}.


%% @private
terminate( Reason, NetState = #net_state{ iface_mod = IfaceMod } ) ->
  IfaceMod:terminate( Reason, NetState ).
  
%%====================================================================
%% Helper functions
%%====================================================================

%% @doc Lists the tokens on a given place from a net state.
%%
%%      Throws an error if the list does not exist.
-spec ls_place( Place :: atom(), NetState :: #net_state{} ) -> [_].

ls_place( Place, #net_state{ marking = Marking } ) ->
  maps:get( Place, Marking ).


-spec get_usr_info( NetState :: #net_state{} ) -> _.

get_usr_info( #net_state{ usr_info = UsrInfo } ) ->
  UsrInfo.


%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Produce the tokens on the places as described in the `ProdMap' argument
%%      atomically in the net instance under process id `Name'.
%%
%%      Note that production succeeds even if a non-existing process is
%%      addressed or the net instance is down.
-spec produce( Name :: name(), ProdMap :: #{ atom() => [_] } ) -> ok.

produce( Name, ProdMap ) ->
  gen_server:cast( Name, {produce, ProdMap} ).


-spec handle_trigger( ProdMap, NetState ) -> #net_state{}
when ProdMap  :: #{ atom() => [_] },
     NetState :: #net_state{}.

handle_trigger( ProdMap, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  G = fun( P, TkLst, Acc ) ->

        F = fun( Tk, A ) ->
              case IfaceMod:trigger( P, Tk, NetState ) of
                pass -> [Tk|A];
                drop -> A
              end
            end,

        TkLst1 = lists:foldl( F, [], TkLst ),
        Acc#{ P => TkLst1 }

      end,

  ProdMap1 = maps:fold( G, #{}, ProdMap ),
  prd( ProdMap1, NetState ).


-spec cns( Mode, NetState ) -> #net_state{}
when Mode     :: #{ atom() => [_] },
     NetState :: #net_state{}.

cns( Mode, NetState = #net_state{ marking = Marking } ) ->

  F = fun( T, TkLst, Acc ) ->
        Acc#{ T => TkLst--maps:get( T, Mode, [] ) }
      end,

  NetState#net_state{ marking = maps:fold( F, #{}, Marking ) }.


-spec prd( ProdMap, NetState ) -> #net_state{}
when ProdMap  :: #{ atom() => [_] },
     NetState :: #net_state{}.

prd( ProdMap, NetState = #net_state{ marking = Marking } ) ->

  F = fun( T, TkLst, Acc ) ->
        Acc#{ T => TkLst++maps:get( T, ProdMap, [] ) }
      end,

  NetState#net_state{ marking = maps:fold( F, #{}, Marking ) }.


-spec progress( NetState :: #net_state{} ) ->
        abort | {delta, #{ atom() => [_]}, #{ atom() => [_] }}.

progress( #net_state{ marking  = Marking,
                      net_mod  = NetMod,
                      usr_info = UsrInfo } ) ->

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
  attempt_progress( ModeMap, NetMod, UsrInfo ).


-spec attempt_progress( ModeMap, NetMod, UsrInfo ) -> abort | {delta, _, _}
when ModeMap :: #{ atom() => [_] },
     NetMod  :: atom(),
     UsrInfo :: _.

attempt_progress( ModeMap, NetMod, UsrInfo ) ->

  case maps:size( ModeMap ) of

    0 -> abort;
    _ ->

      TrsnLst = maps:keys( ModeMap ),
      Trsn = lib_combin:pick_from( TrsnLst ),
      #{ Trsn := ModeLst } = ModeMap,
      Mode = lib_combin:pick_from( ModeLst ),

      case NetMod:fire( Trsn, Mode, UsrInfo ) of

        {produce, ProdMap} ->
          {delta, Mode, ProdMap};
        
        abort ->
          ModeLst1 = ModeLst--[Mode],
          case ModeLst1 of
            []    ->
              attempt_progress( maps:remove( Trsn, ModeMap ), NetMod, UsrInfo );
            [_|_] ->
              attempt_progress( ModeMap#{ Trsn := ModeLst1 }, NetMod, UsrInfo )
          end

      end
  end.


-spec enum_mode( Preset, Marking ) -> [#{ atom() => [_] }]
when Preset  :: [atom()],
     Marking :: #{ atom() => [_] }.

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