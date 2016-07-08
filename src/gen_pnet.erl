%% -*- erlang -*-
%%
%%
%% A generic Petri net OTP library
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
%%
%%
%%
%%--------------------------------------------------------------------
%% @author Jörgen Brandt <brandjoe@hu-berlin.de>
%% @copyright 2016 Jörgen Brandt
%% @doc A module representing a Petri net.
%%
%% To define the structure of a Petri net, users implement a number of
%% callback functions laid out in the gen_pnet module.
%%
%% <h3>User module exports</h3>
%%
%% <h4>init/1</h4>
%% An initialization function called when the gen_pnet module is started.
%% Returns
%% <ul>
%%   <li>the initial marking of the Petri net in the form of a token list</li>
%%   <li>a user-defined data structure that is made available when enumerating
%%       possible consumption lists with `enum_consume_lst/1' and when a
%%       transition is fired with `fire/2'. Note that this user-defined data
%%       structure cannot be updated afterwards.</li>
%% </ul>
%% ```
%%  init( InitArg :: _ )  
%%    -> {ok, [#token{}], _}
%% '''
%% <h4>place_lst/0</h4>
%% The `place_lst' function returns a list of atoms denoting the names of the
%% places in the Petri net's structure.
%% ```
%%  place_lst()  
%%    -> [atom()]
%% '''
%% <h4>trsn_lst/0</h4>
%% The `trsn_lst' function returns a list of atoms denoting the names of the
%% transitions in the Petri net's structure.
%% ```
%%  trsn_lst()  
%%    -> [atom()]
%% '''
%% <h4>preset/1</h4>
%% The `preset' function returns the preset of a given transition. I.e., it
%% enumerates all places, this transition may consume tokens from.
%% ```
%%  preset( Trsn::atom() )  
%%    -> [atom()]
%% '''
%% <h4>enum_consume_lst/3</h4>
%% The `enum_consume_lst' function consumes a map associating a place atom with
%% a list of tokens and returns a list of lists, where each list contains a
%% combination of tokens that can be consumed firing a given transition. Returns
%% the empty list if the transition is not enabled. One of the enumerated lists
%% is chosen to be handed to the `fire/2' function firing the transition.
%% ```
%%  enum_consume_lst( Trsn     :: atom(),
%%                    TokenMap :: #{ atom() => [#token{}]},
%%                    UserInfo :: _ )
%%    -> [[#token{}]]
%% '''
%%
%% <h4>fire/3</h4>
%% Called to fire a transition with a given consumption list. `fire/2' must
%% return the list of tokens that is produced.
%% ```
%%  fire( Trsn       :: atom(),
%%        ConsumeLst :: [#token{}],
%%        UserInfo   :: _ )
%%    -> [#token{}]
%% '''
%%
%% @end
%%--------------------------------------------------------------------

-module( gen_pnet ).

-behaviour( gen_server ).

-export( [start_link/3, start_link/4, stop/1, ls/2, consume/2, produce/2,
          add/3, get_token_map/2] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3] ).

%%====================================================================
%% Callback function definition
%%====================================================================

-callback init( InitArg::_ ) ->
  {ok, InitMarking::#{atom() => [_]}, UserInfo::_}.

-callback place_lst()
  -> PlaceLst::[atom()].

-callback trsn_lst()
  -> TrsnLst::[atom()].

-callback preset( Trsn::atom() )
  -> PlaceLst::[atom()].

-callback enum_consume_map( Trsn     :: atom(),
                            TokenMap :: #{ atom() => [_] },
                            UserInfo :: _)
  -> [#{ atom() => [_] }].

-callback fire( Trsn       :: atom(),
                ConsumeMap :: #{ atom() => [_] },
                UserInfo   :: _ )
  -> ProduceLst::#{ atom() => [_] }.


%%====================================================================
%% Internal record definitions
%%====================================================================

-record( mod_state, { mod, user_info, mgr_map, token_map = #{} } ).


%%====================================================================
%% Data types
%%====================================================================

-type flag()         :: trace
                      | log
                      | {logfile, File::string()}
                      | statistics
                      | debug.

-type option()       :: {timeout, Timeout::pos_integer()}
                      | {debug, [Flag::flag()]}.

-type server_name()  :: {local, atom()}
                      | {global, atom()}
                      | {via, atom(), _}.

-type init_arg()     :: {Mod::atom(), UserArg::_}.


-type call_reply()   :: ok
                      | {ok, [_]}
                      | {ok, #{ atom() => [_] }}
                      | {error, stale_request
                              | no_such_place
                              | unsupported_op}.

-type call_return()  :: {stop, normal, stopped, #mod_state{}}
                      | {reply, call_reply(), #mod_state{}}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an unregistered Petri net instance, returning its process id.

-spec start_link( Mod, UserArg, Options ) -> Result
when Mod     :: atom(),
     UserArg    :: _,
     Options :: [option()],
     Result  :: _.

start_link( Mod, UserArg, Options )
when is_atom( Mod ),
     is_list( Options ) ->

  gen_server:start_link( ?MODULE, {Mod, UserArg}, Options ).

%% @doc Starts a Petri net instance and registers it under a specified name.

-spec start_link( ServerName, Mod, UserArg, Options ) -> Result
when ServerName :: server_name(),
     Mod        :: atom(),
     UserArg    :: _,
     Options    :: [option()],
     Result     :: _.

start_link( ServerName, Mod, UserArg, Options )
when is_tuple( ServerName ),
     is_atom( Mod ),
     is_list( Options ) ->

  start_link( ServerName, ?MODULE, {Mod, UserArg}, Options ).

%% @doc Orderly stops the Petri net.

-spec stop( ServerRef::_ ) -> stopped.

stop( ServerRef ) ->
  gen_server:call( ServerRef, stop ).

%% @doc Lists all tokens associated to a given place.

-spec ls( ServerRef::_, Place::atom() ) ->
  {ok, [_]} | {error, no_such_place}.

ls( ServerRef, Place ) when is_atom( Place ) ->
  gen_server:call( ServerRef, {ls, Place} ).

-spec get_token_map( ServerRef::_, Pl::[atom()] ) -> #{ atom() => [_] }.

get_token_map( ServerRef, Pl ) when is_list( Pl ) ->
  gen_server:call( ServerRef, {get_token_map, Pl} ).

%% @doc Removes a specified list of tokens from the Petri net.

-spec consume( ServerRef::_, ConsumeMap::#{ atom() => [_] } ) ->
  ok | {error, stale_request}.

consume( ServerRef, ConsumeMap ) when is_map( ConsumeMap ) ->
  gen_server:call( ServerRef, {consume, ConsumeMap} ).

%% @see add/2
%% @see consume/3
%% @doc Adds a specified list of tokens to the Petri net.
%%
%% In contrast to `add/2', this function throws an error if one of the tokens
%% specifies a non-existent place causing the Petri net instance to crash.

-spec produce( ServerRef::_, ProduceMap::#{ atom() => [_] } ) -> ok.

produce( ServerRef, ProduceMap ) when is_map( ProduceMap ) ->
  gen_server:call( ServerRef, {produce, ProduceMap} ).


%% @doc Adds a single token to the Petri net.
%% @see produce/2

-spec add( ServerRef::_, Place::atom(), Token::_ ) -> ok.

add( ServerRef, Place, Token ) when is_atom( Place ) ->
  gen_server:call( ServerRef, {add, Place, Token} ).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%% @doc `gen_server' user module implementation of `init/1'.

-spec init( InitArg::init_arg() ) -> {ok, State::#mod_state{}}.

init( {Mod, UserArg} ) when is_atom( Mod ) ->

  io:format( "gen_pnet:init() {~p, ~p} )~n", [Mod, UserArg] ),

  % get initial marking and user info
  {ok, TokenMap, UserInfo} = Mod:init( UserArg ),

  % check if all mentioned places do exist
  ok = lists:foreach( fun( P ) ->
                        case lists:member( P, Mod:place_lst() ) of
                          true  -> ok;
                          false -> error( {no_such_place, P} )
                        end
                      end,
                      maps:keys( TokenMap ) ),

  % check if all existing places were mentioned
  TokenMap1 = lists:foldl( fun( P, Acc ) ->
                        case maps:is_key( P, TokenMap ) of
                          true  -> Acc;
                          false -> Acc#{ P => [] }
                        end
                      end,
                      TokenMap,
                      Mod:place_lst() ),


  F = fun( Place, Acc ) ->

        % start an event manager for each place
        {ok, Pid} = gen_event:start_link(),

        G = fun( Trsn ) ->

              % register an event handler for each transition having this place
              % in its preset
              gen_event:add_handler( Pid,
                                     trsn_handler,
                                     {Mod, UserInfo, self(), Trsn} )
            end,

        ok = lists:foreach( G, which_trsns( Mod, Place ) ),

        Acc#{ Place => Pid }

      end,


  MgrMap = lists:foldl( F, #{}, Mod:place_lst() ),


  % create pnet state
  State    = #mod_state{ mod       = Mod,
                         user_info = UserInfo,
                         mgr_map   = MgrMap,
                         token_map = TokenMap1 },

  {ok, State}.

%% @doc `gen_server' user module implementation of `handle_call/3'.

-spec handle_call( Request, {Tag, Pid}, State ) -> Result
when Request  :: _,
     Tag      :: _,
     Pid      :: pid(),
     State    :: #mod_state{},
     Result   :: call_return().

handle_call( stop, _From, State = #mod_state{} ) ->
  {stop, normal, stopped, State};

handle_call( {get_token_map, Pl}, _From,
             State = #mod_state{ token_map = TokenMap } )
when is_list( Pl ),
     is_map( TokenMap ) ->

  F = fun( P, Acc ) ->
        #{ P := L } = TokenMap,
        Acc#{ P => L }
      end,

  Tm = lists:foldl( F, #{}, Pl ),

  {reply, Tm, State};

handle_call( {ls, Place}, _From,
             State = #mod_state{ mod = Mod, token_map = TokenMap } )
when is_atom( Place ) ->

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      {reply, {error, no_such_place}, State};

    true  ->
      #{ Place := Result } = TokenMap,
      {reply, {ok, Result}, State}

  end;

handle_call( {consume, ConsumeMap}, _From,
             State = #mod_state{ token_map = TokenMap } )
when is_map( ConsumeMap ) ->

  io:format( "gen_pnet:handle_call( {consume, ~p}, _, ~p )~n",
             [ConsumeMap, State] ),

  F = fun( _P, {error, stale_request} ) -> {error, stale_request};
         ( P, Acc ) ->

        #{ P := ConsumeLst } = ConsumeMap,
        #{ P := TokenLst } = TokenMap,

        TokenLst1 = TokenLst--ConsumeLst,
        L = length( TokenLst )-length( ConsumeLst ),
        case length( TokenLst1 ) of

          L ->

            Acc#{ P => TokenLst1 };

          _ ->

            {error, stale_request}

        end
      end,

  case lists:foldl( F, TokenMap, maps:keys( ConsumeMap ) ) of

    {error, stale_request} ->

      io:format( "  !!error: stale_request~n" ),

      {reply, {error, stale_request}, State};

    TokenMap1 when is_map( TokenMap1 ) ->

      io:format( "  **success~n~p~n", [TokenMap1] ),

      {reply, ok, State#mod_state{ token_map = TokenMap1 }}

  end;



handle_call( {add, Place, Token}, _From,
              State = #mod_state{ mod       = Mod,
                                  mgr_map   = MgrMap,
                                  token_map = TokenMap } )
when is_atom( Place ),
     is_atom( Mod ),
     is_map( MgrMap ),
     is_map( TokenMap ) ->

  case lists:member( Place, Mod:place_lst() ) of

    false ->
      {reply, {error, no_such_place}, State};

    true ->

      #{ Place := Mgr } = MgrMap,
      io:format( "notifying mgr ...~n" ),
      gen_event:notify( Mgr, place_update ),

      #{ Place := TokenLst } = TokenMap,
      TokenMap1 = TokenMap#{ Place => [Token|TokenLst] },

      {reply, ok, State#mod_state{ token_map = TokenMap1 }}

  end;

handle_call( {produce, AddMap}, _From,
             State = #mod_state{ mod       = Mod,
                                 mgr_map   = MgrMap,
                                 token_map = TokenMap } )
when is_map( AddMap ),
     is_atom( Mod ),
     is_map( MgrMap ),
     is_map( TokenMap ) ->

  PlaceLst = Mod:place_lst(),
  Pl = maps:keys( AddMap ),

  F = fun( Place, Acc ) ->
        case lists:member( Place, PlaceLst ) of
          false -> error( {no_such_place, Place} );
          true  ->
            #{ Place := AddLst } = AddMap,
            #{ Place := TokenLst } = TokenMap,
            Acc#{ Place => AddLst++TokenLst }
        end
      end,

  TokenMap1 = lists:foldl( F, TokenMap, Pl ),

  G = fun( P ) ->
        #{ P := Mgr } = MgrMap,
        gen_event:notify( Mgr, place_update )
      end,

  ok = lists:foreach( G, Pl ),

  {reply, ok, State#mod_state{ token_map = TokenMap1 }};

handle_call( Request, _From, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_call},
                                {mod, Mod}, {request, Request},
                                {return, {error, unsupported_op}}] ),

  {reply, {error, unsupported_op}, State}.


%% @doc `gen_server' user module implementation of `handle_cast/2'.

-spec handle_cast( Request, State ) -> {noreply, NewState}
when Request  :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_cast( Request, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_cast},
                                {mod, Mod}, {request, Request},
                                {action, ignored}] ),


  {noreply, State}.

%% @doc `gen_server' user module implementation of `handle_info/2'.

-spec handle_info( Info, State ) -> {noreply, NewState}
when Info     :: _,
     State    :: #mod_state{},
     NewState :: #mod_state{}.

handle_info( Info, State = #mod_state{ mod = Mod } ) ->

  error_logger:warning_report( [{module, gen_pnet}, {callback, handle_info},
                                {mod, Mod}, {info, Info},
                                {action, ignored}] ),

  {noreply, State}.

%% @doc `gen_server' user module implementation of `terminate/2'.

-spec terminate( Reason::_, State::#mod_state{} ) -> ok.

terminate( Reason, _State ) ->

  io:format( "gen_pnet:terminate( ~p, _ )~n", [Reason] ),

  ok.

%% @doc `gen_server' user module implementation of `code_change/3'.

-spec code_change( OldVsn, State, Extra ) -> {ok, NewState}
when OldVsn   :: string(),
     State    :: #mod_state{},
     Extra    :: _,
     NewState :: #mod_state{}.

code_change( _OldVsn, State = #mod_state{}, _Extra ) ->
  {ok, State}.


%%====================================================================
%% Internal functions
%%====================================================================

-spec which_trsns( Mod::atom(), Place::atom() ) -> [atom()].

which_trsns( Mod, Place ) when is_atom( Place ) ->
  
  F = fun( Trsn, Acc ) ->
        case lists:member( Place, Mod:preset( Trsn ) ) of
          false -> Acc;
          true  -> [Trsn|Acc]
        end
      end,

  lists:foldl( F, [], Mod:trsn_lst() ).
