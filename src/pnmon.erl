-module( pnmon ).
-behaviour( gen_event ).

-export( [start_link/0, notify/1] ).

-export( [code_change/3, handle_call/2, handle_info/2, init/1, terminate/2,
          handle_event/2] ).

-define( SERVER, ?MODULE ).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, Pid::pid()}.

start_link() ->
  {ok, Pid} = gen_event:start_link( {local, ?SERVER} ),
  ok = gen_event:add_handler( ?SERVER, ?MODULE, [] ),
  {ok, Pid}.

-spec notify( Event::tuple() ) -> ok.

notify( Event ) when is_tuple( Event ) ->
  gen_event:notify( ?SERVER, Event ).

%%====================================================================
%% gen_event callback functions
%%====================================================================

-spec code_change( OldVsn::string(), State::_, Extra::_ ) -> {ok, NewState::_}.
code_change( _OldVsn, State, _Extra ) -> {ok, State}.


-spec handle_call( Request::_, State::_ ) -> {ok, Reply::_, NewState::_}.
handle_call( _Request, State ) -> {ok, ok, State}.

-spec handle_info( Info::_, State::_ ) -> {ok, NewState::_}.
handle_info( _Info, State ) -> {ok, State}.

-spec init( Args::_ ) -> {ok, State::_}.
init( _Args ) -> {ok, []}.

-spec terminate( Arg::_, State::_ ) -> ok.
terminate( _Arg, _State ) -> ok.


-spec handle_event( Event::tuple(), State::_ ) -> {ok, NewState::_}.

handle_event( Event, State ) when is_tuple( Event ) ->
  io:format( "~p~n", [Event] ),
  {ok, State}.