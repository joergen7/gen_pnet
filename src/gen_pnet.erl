%% -*- erlang -*-
%%
%% A generic Petri net OTP behavior.
%%
%% Copyright 2016-2017 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.6
%% @copyright 2016-2017 Jörgen Brandt
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
-export( [start_link/3, start_link/4, ls/2, marking/1, call/2, call/3,
          cast/2, stats/1, reply/2, reset_stats/1, stop/1, usr_info/1] ).

% Net state constructor and accessor functions
-export( [new/2, get_ls/2, get_usr_info/1, get_stats/1] ).

% gen_server callbacks
-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          init/1, terminate/2] ).


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

-type start_link_result() :: {ok, pid()}
                | ignore
                | {error, _}.

-type handle_call_request() :: {ls, atom()}
                             | marking
                             | usr_info
                             | {call, _}
                             | stats
                             | reset_stats.

-type handle_call_result() :: {reply, _, #net_state{}}
                            | {noreply, #net_state{}}
                            | {stop, _, _, #net_state{}}.


-type handle_cast_request() :: continue
                             | {cast, _}.

-type handle_cast_result() :: {noreply, #net_state{}}
                            | {stop, _, #net_state{}}.

-type handle_info_result() :: {noreply, #net_state{}}
                            | {stop, _, #net_state{}}.

-type prop() :: atom() | {atom(), _}.

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

-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
            boolean().

-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an unregistered net instance.
%% @see start_link/4
-spec start_link( IfaceMod, Args, Options ) -> start_link_result()
when IfaceMod :: atom(),
     Args     :: _,
     Options  :: [prop()].

start_link( IfaceMod, Args, Options )
when is_atom( IfaceMod ), is_list( Options ) ->
  gen_server:start_link( ?MODULE, {IfaceMod, Args}, Options ).

%% @doc Starts a net instance registered as `ServerName' using the callback
%%      module `IfaceMod' as the interface module for this net instance.
%%
%%      The `Args' argument is later handed to the `init/1' callback. The
%%      `ServerName' argument can be
%%      `{local, Name} | {global, Name} | {via, Module, ViaName}'. Internally,
%%      the server name `ServerName' and option list `Options' are handed down
%%      to `gen_server:start_link/4' as is.
%%
%% @see init/1
-spec start_link( ServerName, IfaceMod, Args, Options ) -> start_link_result()
when ServerName :: server_name(),
     IfaceMod   :: atom(),
     Args       :: _,
     Options    :: [prop()].

start_link( ServerName, IfaceMod, Args, Options )
when is_tuple( ServerName ), is_atom( IfaceMod ), is_list( Options ) ->
  gen_server:start_link( ServerName, ?MODULE, {IfaceMod, Args}, Options ).

%% @doc Query the list of tokens on the place named `Place' in the net instance
%%      identified as `Name'.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is either `{ok, [_]}'' if the place exists or a
%%      `{error, #bad_place{}}' tuple.
-spec ls( Name, Place ) -> {ok, [_]} | {error, #bad_place{}}
when Name  :: name(),
     Place :: atom().

ls( Name, Place ) when is_atom( Place ) -> gen_server:call( Name, {ls, Place} ).

%% @doc Query the marking map of the net instance identified as `Name'
%%      associating to each place name the list of tokens that this place holds.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is the Petri net's marking map.
-spec marking( Name :: name() ) -> #{ atom() => [_] }.

marking( Name ) -> gen_server:call( Name, marking ).

%% @doc Query the user info term from the net instance identified as `Name'.
-spec usr_info( Name :: name() ) -> _.

usr_info( Name ) -> gen_server:call( Name, usr_info ).

%% @doc Query the statistics gathered by the net instance identified as `Name'.
%%
%%      The throughput is given as a `#stats{}' record consisting of three
%%      `#stat{}' record instances characterizing the current, maximum, and
%%      minimum throughput of this net in transition firings per second.
-spec stats( Name :: name() ) -> #stats{}.

stats( Name ) -> gen_server:call( Name, stats ).

%% @doc Requests the net instance identified as `Name' to clear its stats.
-spec reset_stats( Name :: name() ) -> ok.

reset_stats( Name ) -> gen_server:call( Name, reset_stats ).

%% @doc Requests the net instance identified as `Name' to stop.
-spec stop( Name :: name() ) -> ok.

stop( Name ) -> gen_server:stop( Name ).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.

%%      The timeout is implicitly set to five seconds.
%% @see call/3
-spec call( Name :: name(), Request :: _ ) -> _.

call( Name, Request ) -> gen_server:call( Name, {call, Request} ).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.
%%
%%      The timeout is explicitly set to `Timeout`. The request is handled by
%%      the `handle_call/3' callback function of the interface module.
-spec call( Name :: name(), Request :: _, Timeout :: non_neg_integer() ) -> _.

call( Name, Request, Timeout ) when is_integer( Timeout ), Timeout >= 0 ->
  gen_server:call( Name, {call, Request}, Timeout ).

%% @doc Asynchronously send the term `Request' to the net instance identified as
%%      `Name'.
%%
%%      The request is handled by the `handle_cast/2' callback function of the
%%      interface module. Note that the cast succeeds even if a non-existing
%%      process is addressed or the net instance is down.
-spec cast( Name :: name(), Request :: _ ) -> ok.

cast( Name, Request ) ->
  gen_server:cast( Name, {cast, Request} ).

%% @doc Sends a reply to a calling client process.
%%
%%      This funciton is to be used when the reply to a caller has been
%%      deferred by returning `{noreply, _, _}' in `handle_call/3'.
%% @see handle_call/3
-spec reply( Client :: {pid(), _}, Reply :: _ ) -> _.

reply( Client, Reply ) when is_tuple( Client ) ->
  gen_server:reply( Client, Reply ).


%%====================================================================
%% Net state constructor and accessor functions
%%====================================================================

%% @doc Constructs an initial instance of a state record.
%%
%%      Such a state record can be used to initialize a `gen_pnet' instance with
%%      `start_link/1' or `start_link/2'.
%%
%% @see start_link/2
%% @see start_link/3
-spec new( NetMod :: atom(), UsrInfo :: _ ) -> #net_state{}.

new( NetMod, UsrInfo ) when is_atom( NetMod ) ->
  #net_state{ net_mod = NetMod, usr_info = UsrInfo }.


%% @doc Extracts the list of tokens on a given place from a given net state.
%%
%%      Throws an error if the list does not exist.
-spec get_ls( Place :: atom(), NetState :: #net_state{} ) -> [_].

get_ls( Place, #net_state{ marking = Marking } ) -> maps:get( Place, Marking ).

%% @doc Extracts the user info field from a given net state.
-spec get_usr_info( NetState :: #net_state{} ) -> _.

get_usr_info( #net_state{ usr_info = UsrInfo } ) -> UsrInfo.


%% @doc Extracts the stats field from a given net instance.
-spec get_stats( NetState :: #net_state{} ) -> #stats{}.

get_stats( #net_state{ stats = Stats } ) -> Stats.


%%====================================================================
%% Generic server callback functions
%%====================================================================

%% @private
-spec code_change( OldVsn, NetState, Extra ) -> {ok, #net_state{}} | {error, _}
when OldVsn   :: _,
     NetState :: #net_state{},
     Extra    :: _.

code_change( OldVsn, NetState = #net_state{ iface_mod = IfaceMod }, Extra ) ->
  IfaceMod:code_change( OldVsn, NetState, Extra ).


%% @private
-spec handle_call( Request, From, NetState ) -> handle_call_result()
when Request  :: handle_call_request(),
     From     :: {pid(), _},
     NetState :: #net_state{}.

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
      NetState2 = handle_trigger( ProdMap, NetState1 ),
      continue( self() ),
      {reply, Reply, NetState2};

    noreply ->
      {noreply, NetState};

    {noreply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      NetState2 = handle_trigger( ProdMap, NetState1 ),
      continue( self() ),
      {noreply, NetState2};

    {stop, Reason, Reply} ->
      {stop, Reason, Reply, NetState}

  end;

handle_call( stats, _From, NetState = #net_state{ stats = Stats } ) ->
  {reply, Stats, NetState};

handle_call( reset_stats, _From, NetState ) ->
  {reply, ok, NetState#net_state{ stats = undefined }}.


%% @private
-spec handle_cast( Request, NetState ) -> handle_cast_result()
when Request  :: handle_cast_request(),
     NetState :: #net_state{}.

handle_cast( continue,
             NetState = #net_state{ stats  = Stats,
                                    tstart = T1,
                                    cnt    = Cnt } ) ->
  
  case progress( NetState ) of

    abort ->
      {noreply, NetState};

    {delta, Mode, Pm} ->

      NetState1 = cns( Mode, NetState ),
      NetState2 = handle_trigger( Pm, NetState1 ),
      continue( self() ),

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
      NetState2 = handle_trigger( ProdMap, NetState1 ),
      continue( self() ),
      {noreply, NetState2};

    {stop, Reason} ->
      {stop, Reason, NetState}

  end.


%% @private
-spec handle_info( Info, NetState ) -> handle_info_result()
when Info     :: _,
     NetState :: #net_state{}.

handle_info( Info, NetState = #net_state{ iface_mod = IfaceMod } ) ->

  case IfaceMod:handle_info( Info, NetState ) of

    noreply ->
      {noreply, NetState};

    {noreply, CnsMap, ProdMap} ->
      NetState1 = cns( CnsMap, NetState ),
      NetState2 = handle_trigger( ProdMap, NetState1 ),
      continue( self() ),
      {noreply, NetState2};

    {stop, Reason} ->
      {stop, Reason, NetState}

  end.


%% @private
-spec init( Args :: {atom(), _} ) -> {ok, #net_state{}}.

init( {IfaceMod, Args} ) ->

  {ok, NetState} = IfaceMod:init( Args ),
  #net_state{ net_mod = NetMod, usr_info = UsrInfo } = NetState,

  PlaceLst = NetMod:place_lst(),

  F = fun( P, Acc ) ->
        Acc#{ P => NetMod:init_marking( P, UsrInfo ) }
      end,

  InitMarking = lists:foldl( F, #{}, PlaceLst ),

  continue( self() ),

  {ok, NetState#net_state{ iface_mod = IfaceMod,
                           marking   = InitMarking,
                           stats     = undefined,
                           tstart    = os:system_time(),
                           cnt       = 0 }}.


%% @private
-spec terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

terminate( Reason, NetState = #net_state{ iface_mod = IfaceMod } ) ->
  IfaceMod:terminate( Reason, NetState ).
  

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Continue making progress in net instance under process id `Name'.
%%
%%      Note that continuing succeeds even if a non-existing process is
%%      addressed or the net instance is down.
-spec continue( Name :: name() ) -> ok.

continue( Name ) ->
  gen_server:cast( Name, continue ).


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
        IsEnabled = fun( M ) -> NetMod:is_enabled( T, M, UsrInfo ) end,
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