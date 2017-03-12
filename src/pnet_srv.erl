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

-module( pnet_srv ).
-behavior( gen_server ).



-export( [init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3] ).

-export( [start_link/2, start_link/3, ls/2, call/2, cast/2, produce/2,
          produce_token_lst/3, produce_token/3] ).

%%====================================================================
%% Gen_server callback functions
%%====================================================================

init( {IfaceMod, NetMod} ) ->

  PlaceLst = NetMod:place_lst(),
  TriggerMap = IfaceMod:trigger_map(),

  % initialize empty place map
  F = fun( P, Acc ) -> Acc#{ P => [] } end,
  EmptyPlaceMap = lists:foldl( F, #{}, PlaceLst ),

  % fetch initial marking and check it
  PlaceMap = case NetMod:init_marking() of

               pass                   -> EmptyPlaceMap;
               {produce, InitMarking} ->

                 ReducedMarking = apply_triggers( InitMarking, TriggerMap ),

                 % add initial marking to empty place map
                 case check_produce( ReducedMarking, EmptyPlaceMap ) of
                   {ok, M}         -> M;
                   {error, Reason} -> error( Reason )
                 end

             end,


  % start transition supervisor
  supervisor:start_link( trsn_sup, NetMod ),

  {ok, {IfaceMod, NetMod, PlaceMap}}.

terminate( _Reason, _State ) ->
  ok.

handle_call( {call, Request}, From, State={IfaceMod, NetMod, PlaceMap} ) ->

  TriggerMap = IfaceMod:trigger_map(),

  case IfaceMod:handle_call( Request, From ) of

    {pass, Reply}             -> {reply, Reply, State};
    {produce, ProdMap, Reply} ->

      ReducedProdMap = apply_triggers( ProdMap, TriggerMap ),

      case check_produce( ReducedProdMap, PlaceMap ) of
        {error, Reason}   -> error( Reason );
        {ok, NewPlaceMap} -> {reply, Reply, {IfaceMod, NetMod, NewPlaceMap}}
      end

  end;

handle_call( {ls, Place}, _From, State={_, PlaceMap} ) ->
  
  case maps:is_key( Place, PlaceMap ) of
    true  -> {reply, {ok, maps:get( Place, PlaceMap )}, State};
    false -> {reply, {error, {bad_place, Place}}, State}
  end;

handle_call( {produce, ProdMap}, _From, State={IfaceMod, NetMod, PlaceMap} ) ->

  TriggerMap = IfaceMod:trigger_map(),
  ReducedProdMap = apply_triggers( ProdMap, TriggerMap ),

  case check_produce( ReducedProdMap, PlaceMap ) of
    {error, Reason} -> {reply, {error, Reason}, State};
    {ok, PlaceMap1} -> {reply, ok, {IfaceMod, NetMod, PlaceMap1}}
  end;

handle_call( {fire, ConsumeMap, ProdMap}, _From,
             State={IfaceMod, NetMod, PlaceMap} ) ->

  TriggerMap = IfaceMod:trigger_map(),

  case check_consume( ConsumeMap, PlaceMap ) of

    {error, token_missing} -> {reply, {error, token_missing}, State};
    {error, Reason1}       -> error( Reason1 );
    {ok, PlaceMap1}        ->

      ReducedProdMap = apply_triggers( ProdMap, TriggerMap ),

      case check_produce( ReducedProdMap, PlaceMap1 ) of
        {error, Reason2} -> error( Reason2 );
        {ok, PlaceMap2}  -> {reply, ok, {IfaceMod, NetMod, PlaceMap2}}
      end

  end;

handle_call( {mode_lst, Trsn}, _From, State={_, NetMod, PlaceMap} ) ->

  Preset = NetMod:preset( Trsn ),

  Folder = fun( P, TokenLst, Acc ) ->
             case lists:member( P, Preset ) of
               false -> Acc;
               true  -> Acc#{ P => TokenLst }
             end
           end,

  RelevantMap = maps:fold( Folder, #{}, PlaceMap ),

  ModeLst = NetMod:mode_lst( Trsn, RelevantMap ),

  {reply, {ok, ModeLst}, State}; 

handle_call( Request, _From, State ) ->
  {reply, {error, {bad_request, Request}}, State}.

handle_cast( {cast, Request}, State={IfaceMod, NetMod, PlaceMap} ) ->
  
  TriggerMap = IfaceMod:trigger_map(),

  case IfaceMod:handle_cast( Request ) of

    pass               -> {noreply, State};
    {produce, ProdMap} ->

      ReducedProdMap = apply_triggers( ProdMap, TriggerMap ),

      case check_produce( ReducedProdMap, PlaceMap ) of
        {error, Reason}   -> error( Reason );
        {ok, NewPlaceMap} -> {noreply, {IfaceMod, NetMod, NewPlaceMap}}
      end

  end;

handle_cast( _Request, State ) ->
  {noreply, State}.

handle_info( _Info, State ) ->
  {noreply, State}.  

code_change( _OldVsn, State, _Extra ) ->
  {ok, State}.


%%====================================================================
%% API functions
%%====================================================================

start_link( IfaceMod, NetMod ) ->
  gen_server:start_link( pnet_srv, {IfaceMod, NetMod}, [] ).

start_link( ServerName, IfaceMod, NetMod ) ->
  gen_server:start_link( ServerName, pnet_srv, {IfaceMod, NetMod}, [] ).

ls( Pid, Place ) ->
  gen_server:call( Pid, {ls, Place} ).

call( Pid, Request ) ->
  gen_server:call( Pid, {call, Request} ).

cast( Pid, Request ) ->
  gen_server:cast( Pid, {cast, Request} ).

produce( Pid, ProdMap ) ->
  gen_server:call( Pid, {produce, ProdMap} ).

produce_token_lst( Pid, Place, TokenLst ) ->
  produce( Pid, #{ Place => TokenLst } ).

produce_token( Pid, Place, Token ) ->
  produce( Pid, #{ Place => [Token] } ).

%%====================================================================
%% Internal functions
%%====================================================================


-spec invalid_place_lst( TestMap, PlaceMap ) -> [atom()]
when TestMap  :: #{ atom() => _ },
     PlaceMap :: #{ atom() => _ }.

invalid_place_lst( TestMap, PlaceMap ) ->

  PlaceLst = maps:keys( PlaceMap ),

  Pred = fun( P, Acc ) ->
           case lists:member( P, PlaceLst ) of
             true  -> Acc;
             false -> [P|Acc]
           end
         end,

  lists:foldl( Pred, [], maps:keys( TestMap ) ).



-spec check_produce( ProdMap, PlaceMap ) -> Result
when ProdMap  :: #{ atom() => [_] },
     PlaceMap :: #{ atom() => [_] },
     Result   :: {ok, #{atom() => [_] }} | {error, {bad_places, [atom()]}}.

check_produce( ProdMap, PlaceMap ) ->

  Folder = fun( P, TokenLst, Acc ) ->
             case maps:is_key( P, ProdMap ) of
               false -> Acc#{ P => TokenLst };
               true  -> Acc#{ P => TokenLst++maps:get( P, ProdMap ) }
             end
           end,

  InvalidPlaceLst = invalid_place_lst( ProdMap, PlaceMap ),

  case InvalidPlaceLst of
    [_|_] -> {error, {bad_places, InvalidPlaceLst}};
    []    -> {ok, maps:fold( Folder, #{}, PlaceMap )}
  end.


-spec check_consume( ConsumeMap, PlaceMap ) -> Result
when ConsumeMap :: #{ atom() => [_] },
     PlaceMap   :: #{ atom() => [_] },
     Result     :: {ok, #{ atom() => [_] }}
                 | {error, bad_places, [atom()]}
                 | {error, token_missing}.

check_consume( ConsumeMap, PlaceMap ) ->

  PredFolder = fun( P, TokenLst, Acc ) ->

                 Pred = fun( Token ) ->
                          lists:member( Token, maps:get( P, PlaceMap ) )
                        end,

                 P = lists:all( Pred, TokenLst ),

                 [P|Acc]

               end,

  ConsumeFolder = fun( P, TokenLst, Acc ) ->

                    case maps:is_key( P, ConsumeMap ) of
                      false -> Acc#{ P => TokenLst };
                      true  -> Acc#{ P => TokenLst--maps:get( P, ConsumeMap ) }
                    end

                  end,


  InvalidPlaceLst = invalid_place_lst( ConsumeMap, PlaceMap ),

  case InvalidPlaceLst of

    [_|_] -> {error, {bad_places, InvalidPlaceLst}};
    []    ->

      TokenPresent = lists:all( maps:fold( PredFolder, [], ConsumeMap ) ),

      case TokenPresent of
        false -> {error, token_missing};
        true  -> {ok, maps:fold( ConsumeFolder, #{}, PlaceMap )}
      end

  end.

apply_triggers( ProduceMap, TriggerMap ) ->

  Folder = fun( P, TokenLst, Acc ) ->

             case maps:is_key( P, TriggerMap ) of
               false -> Acc#{ P => TokenLst };
               true  ->
                 lists:foreach( maps:get( P, TriggerMap ), TokenLst ),
                 Acc
             end
           end,

  maps:fold( Folder, #{}, ProduceMap ).





