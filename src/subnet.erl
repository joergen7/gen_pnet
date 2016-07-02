-module( subnet ).
-behaviour( gen_server ).

-export( [start_link/1, stop/1] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3] ).

%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, place_tab} ).
-record( init_arg, { mod } ).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link( Mod::atom() ) -> {ok, Pid::pid()}.

start_link( Mod ) when is_atom( Mod ) ->
  gen_server:start_link( ?MODULE, #init_arg{ mod = Mod }, [] ).

-spec stop( Pid::pid() ) -> ok.

stop( Pid ) when is_pid( Pid ) ->
  gen_server:call( Pid, stop ).


%%====================================================================
%% gen_server callback functions
%%====================================================================

-spec init( InitArg::#init_arg{} ) -> {ok, State::#mod_state{}}.

init( #init_arg{ mod = Mod } )
when is_atom( Mod ) ->

  PlaceTab = ets:new( place_tab, [] ),
  State    = #mod_state{ mod = Mod, place_tab = PlaceTab },

  {ok, State}.

-spec handle_call( stop, {Tag, Pid}, State ) -> {stop, normal, ok, NewState}
when Tag      :: _,
     Pid      :: pid(),
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_call( stop, _From, State ) when is_tuple( State ) ->
  {stop, normal, ok, State}.

-spec handle_cast( Request, State ) -> {noreply, NewState}
when Request  :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_cast( _Request, State ) when is_tuple( State ) ->
  {noreply, State}.

-spec handle_info( Info, State ) -> {noreply, NewState}
when Info     :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_info( _Info, State ) when is_tuple( State ) ->
  {noreply, State}.

-spec terminate( Reason::_, State::#mod_state{} ) -> ok.

terminate( Reason, #mod_state{ mod = Mod } ) ->
  error_logger:info_report(
    [terminate, {reason, Reason}, {type, subnet}, {pid, self()}, {mod, Mod}] ),
  ok.

-spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
when OldVsn   :: string(),
     State    :: #mod_state{},
     Extra    :: _,
     NewState :: #mod_state{}.

code_change( _OldVsn, State, _Extra ) -> {ok, State}.