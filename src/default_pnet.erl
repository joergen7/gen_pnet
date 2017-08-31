-module( default_pnet ).
-behaviour( gen_pnet ).


%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3,
          fire/3] ).

-export( [start_link/0, start_link/1] ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).


%%====================================================================
%% API functions
%%====================================================================


-spec start_link() -> gen_pnet:start_link_result().

start_link() -> gen_pnet:start_link( ?MODULE, [], [] ).


-spec start_link( ServerName ) -> gen_pnet:start_link_result()
when ServerName :: gen_pnet:server_name().

start_link( ServerName ) -> gen_pnet:start_link( ServerName, ?MODULE, [], [] ).


%%====================================================================
%% Interface callback functions
%%====================================================================

-spec code_change( OldVsn :: _, NetState :: #net_state{}, Extra :: _ ) ->
        {ok, #net_state{}} | {error, _}.

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.


-spec handle_call( Request :: _, From :: {pid(), _},
                       NetState :: #net_state{} ) ->
              {reply, _}
            | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}
            | noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _, _}.

handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.


-spec handle_cast( Request :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_cast( _Request, _NetState ) -> noreply.


-spec handle_info( Info :: _, NetState :: #net_state{} ) ->
              noreply
            | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}
            | {stop, _}.

handle_info( _Request, _NetState ) -> noreply.


-spec init( Args :: _ ) -> {ok, #net_state{}}.

init( _Args ) -> {ok, gen_pnet:new( ?MODULE, [] )}.


-spec terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

terminate( _Reason, _NetState ) -> ok.


-spec trigger( Place :: atom(), Token :: _, NetState :: #net_state{} ) ->
            pass | drop.

trigger( _Place, _Token, _NetState ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

-spec place_lst() -> [atom()].

place_lst() -> [].


-spec trsn_lst() -> [atom()].

trsn_lst() -> [].


-spec init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].

init_marking( _Place, _UsrInfo ) -> [].


-spec preset( Trsn :: atom() ) -> [atom()].

preset( _Trsn ) -> [].


-spec is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
        boolean().

is_enabled( _Trsn, _Mode, _UsrInfo ) -> false.


-spec fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.

fire( _Trsn, _Mode, _UsrInfo ) -> abort.


