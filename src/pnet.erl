-module( pnet ).
-behaviour( gen_server ).

-export( [start/1, start_link/1, stop/1] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3] ).

-include( "pnet.hrl" ).

%%====================================================================
%% Callback function definition
%%====================================================================

-callback place_lst() -> [Place::atom()].
-callback subscr_map() -> #{ TrsnLabel::atom() => [Place::atom()] }.
-callback enum_consume_lst( #{ Place::atom() => [#token{}] } ) -> [[#token{}]].
-callback fire( ConsTokenLst::[#token{}] ) -> ProdTokenLst::[#token{}].

%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, place_lst = [], lock_lst = [] } ).

%%====================================================================
%% API functions
%%====================================================================

-spec start( Mod::atom() ) -> {ok, Pid::pid()}.

start( Mod ) when is_atom( Mod ) ->

  ChildSpec = #{ id       => make_ref(),
                 start    => {?MODULE, start_link, [Mod]},
                 restart  => permanent,
                 shutdown => 5000,
                 type     => worker,
                 modules  => [Mod]
               },

  {ok, _Pid} = supervisor:start_child( pnet_sup, ChildSpec ).

-spec start_link( Mod::atom() ) -> {ok, Pid::pid()}.

start_link( Mod ) when is_atom( Mod ) ->
  gen_server:start_link( ?MODULE, [Mod], [] ).

-spec stop( Pid::pid() ) -> ok.

stop( Pid ) when is_pid( Pid ) ->
  gen_server:call( Pid, stop ).

-spec ls( Pid::pid(), Place::atom() ) -> {ok, [#token{}]}.

ls( Pid, Place )
when is_pid( Pid ),
     is_atom( Place ) ->
  gen_server:call( Pid, {ls, Place} ).


%%====================================================================
%% gen_server callback functions
%%====================================================================

-spec init( InitArg::[atom()] ) -> {ok, State::#mod_state{}}.

init( [Mod] ) when is_atom( Mod ) ->

  % register event
  ok = pnmon:notify( #pnet_init_event{ pid = self(), mod = Mod } ),

  % create pnet state
  State    = #mod_state{ mod = Mod },

  {ok, State}.

-spec handle_call( Request, {Tag, Pid}, State ) -> {stop, normal, ok, NewState}
when Request  :: _,
     Tag      :: _,
     Pid      :: pid(),
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_call( stop, _From, State = #mod_state{} ) ->
  {stop, normal, ok, State};

handle_call( {ls, Place}, _From, State = #mod_state{ mod = Mod, place_lst = PlaceLst } )
when is_atom( Place ) ->

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      error( no_such_place );

    true  ->
      TokenLst = [X || X = #token{ place = P } <- PlaceLst, P =:= Place],
      {reply, {ok, TokenLst}, State}

  end.

-spec handle_cast( Request, State ) -> {noreply, NewState}
when Request  :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_cast( Request, State = #mod_state{} ) ->

  error( cast_unsupported ).

-spec handle_info( Info, State ) -> {noreply, NewState}
when Info     :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_info( Token = #token{ place = Place },
             State = #mod_state{ mod = Mod, place_lst = PlaceLst } )
when is_atom( Place ),
     is_atom( Mod ) ->

  pnmon:notify( #token_recv_event{ pid = self(), mod = Mod, token = Token } ),

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      error( no_such_place );

    true ->
      {noreply, State#mod_state{ place_lst = PlaceLst1 }}

  end.


-spec terminate( Reason::_, State::#mod_state{} ) -> ok.

terminate( Reason, #mod_state{ mod = Mod } ) ->

  pnmon:notify( #pnet_terminate_event{ pid    = self(),
                                       mod    = Mod,
                                       reason = Reason } ),

  ok.

-spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
when OldVsn   :: string(),
     State    :: #mod_state{},
     Extra    :: _,
     NewState :: #mod_state{}.

code_change( _OldVsn, State, _Extra ) -> {ok, State}.