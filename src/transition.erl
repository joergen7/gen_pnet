-module( transition ).
-behaviour( gen_server ).

-export( [start_link/3, stop/1] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3] ).

-include( "pnet.hrl" ).

%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { subnet_pid, mod, transition } ).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link( SubnetPid, Mod, Transition ) -> {ok, Pid}
when SubnetPid  :: pid(),
     Mod        :: atom(),
     Transition :: atom(),
     Pid        :: pid().

start_link( SubnetPid, Mod, Transition )
when is_pid( SubnetPid ),
     is_atom( Mod ),
     is_atom( Transition ) ->

  InitArg = #mod_state{ subnet_pid = SubnetPid,
                        mod        = Mod,
                        transition = Transition },

  gen_server:start_link( ?MODULE, InitArg, [] ).

-spec stop( Pid::pid() ) -> ok.

stop( Pid ) when is_pid( Pid ) ->
  gen_server:call( Pid, stop ).

%%====================================================================
%% gen_server callback functions
%%====================================================================

-spec init( InitState::#mod_state{} ) -> {ok, State::#mod_state{}}.

init( #mod_state{ subnet_pid = SubnetPid, mod = Mod, transition = Transition } )
when is_pid( SubnetPid ),
     is_atom( Mod ),
     is_atom( Transition ) ->

  error_logger:info_report(
    [started,
     {pid, self()},
     {subnet_pid, SubnetPid},
     {mod, Mod},
     {transition, Transition}] ),

  {ok, #mod_state{ subnet_pid = SubnetPid, mod = Mod, transition = Transition }}.

-spec handle_call( stop, {Tag, Pid}, State ) -> {stop, normal, ok, NewState}
when Tag      :: _,
     Pid      :: pid(),
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_call( stop, _From, State ) -> {stop, normal, ok, State}.

-spec handle_cast( {notify, TokenLst}, State ) -> {noreply, NewState}
when TokenLst :: [#token{}],
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_cast( {notify, TokenLst}, State )
when is_list( TokenLst ),
     is_tuple( State ) ->

  handle_notify( TokenLst, State ),

  {noreply, State}.

-spec handle_info( Info, State ) -> {noreply, NewState}
when Info :: _,
     State :: #mod_state{},
     NewState :: #mod_state{}.

handle_info( _Info, State ) -> {noreply, State}.

-spec terminate( Reason::_, State::#mod_state{} ) -> ok.

terminate( Reason, _State ) ->
  error_logger:info_report( [terminated, {pid, self()}, {reason, Reason}] ),
  ok.

-spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
when OldVsn   :: string(),
     State    :: #mod_state{},
     Extra    :: _,
     NewState :: #mod_state{}.

code_change( _OldVsn, State, _Extra ) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec handle_notify( TokenLst, State ) -> ok
when TokenLst :: [#token{}],
     State    :: #mod_state{}.

handle_notify( TokenLst, #mod_state{ subnet_pid = SubnetPid,
                                     mod        = Mod,
                                     transition = Transition } )
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
