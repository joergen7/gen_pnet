-module( trsn ).
-behaviour( gen_event ).

-export( [init/1, handle_call/2, handle_info/2, terminate/2, handle_event/2,
          code_change/3] ).

-include( "pnet.hrl" ).

%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, pnet_pid, label } ).

%%====================================================================
%% gen_server callback functions
%%====================================================================

-spec init( [Mod, PnetPid, Label] ) -> {ok, State}
when Mod     :: atom(),
     PnetPid :: pid(),
     Label   :: atom(),
     State   :: #mod_state{}.

init( [Mod, PnetPid, Label] )
when is_atom( Mod ),
     is_pid( PnetPid ),
     is_atom( Label ) ->

  ok = pnmon:notify( #trsn_init_event{ pid      = self(),
                                       mod      = Mod,
                                       pnet_pid = PnetPid,
                                       label    = Label } ),

  {ok, #mod_state{ mod = Mod, pnet_pid = PnetPid, label = Label }}.

-spec terminate( Arg::_, State::#mod_state{} ) -> ok.

terminate( Arg, #mod_state{ mod = Mod, pnet_pid = PnetPid, label = Label } ) ->

  ok = pnmon:notify( #trsn_terminate_event{ pid      = self(),
                                            mod      = Mod,
                                            pnet_pid = PnetPid,
                                            label    = Label,
                                            reason   = Arg } ).

handle_info( _Info, State )           -> {ok, State}.
code_change( _OldVsn, State, _Extra ) -> {ok, State}.
handle_call( _Request, State )        -> {ok, ok, State}.
handle_event( _Event, State )         -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec handle_notify( TokenLst, State ) -> ok
when TokenLst :: [#token{}],
     State    :: #mod_state{}.

handle_notify( TokenLst, #mod_state{ mod = Mod, pnet_pid = SubnetPid,
                                     label = Transition } )
when is_list( TokenLst ) ->

  % get consume list
  ConsumeLstLst = Mod:consume_lst( Transition, TokenLst ),

  % choose tokens to be consumed
  I = rand:uniform( length( ConsumeLstLst ) ),
  ConsumeLst = lists:nth( I, ConsumeLstLst ),

  % try to get lock on consumable tokens
  case subnet:lock( SubnetPid, ConsumeLst ) of

    {error, unavailable} ->
      error_logger:info_report(
        [lock_unavailable,
         {consume_lst, ConsumeLst},
         {subnet_pid, SubnetPid},
         {mod, Mod},
         {transition, Transition}] );

    ok ->

      error_logger:info_report(
        [lock_acquired,
         {consume_lst, ConsumeLst},
         {subnet_pid, SubnetPid},
         {mod, Mod},
         {transition, Transition}] ),

      % fire transition
      ProduceLst = Mod:produce( Transition, ConsumeLst ),

      % release lock
      subnet:release_lock( ConsumeLst, ProduceLst ),

      error_logger:info_report(
        [lock_released,
         {consume_lst, ConsumeLst},
         {produce_lst, ProduceLst},
         {subnet_pid, SubnetPid},
         {mod, Mod},
         {transition, Transition}] )

  end.
