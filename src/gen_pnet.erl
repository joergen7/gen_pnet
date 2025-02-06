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
%% @version 0.1.7
%% @copyright 2016-2017 Jörgen Brandt
%%
%% @doc Callback function definitions and API for the `gen_pnet' behavior.
%%
%% <h3>Net Structure Callback Functions</h3>
%%
%% There are six callbacks that define the Petri net structure and its initial
%% marking:
%%
%% <ul>
%%   <li>`place_lst/0' returns the names of the places in the net</li>
%%   <li>`trsn_lst/0' returns the names of the transitions in the net</li>
%%   <li>`init_marking/2' returns the initial marking for a given place</li>
%%   <li>`preset/1' returns the preset places of a given transition</li>
%%   <li>`is_enabled/3' determines whether a given transition is enabled in a
%%       given mode</li>
%%   <li>`fire/3' returns which tokens are produced on what places if a given
%%       transition is fired in a given mode that enables this transition</li>
%% </ul>
%%
%% We have a look at each of them in turn.
%%
%% <h4>place_lst/0</h4>
%%
%% The `place_lst/0' function lets us define the names of all places in the net.
%%
%% Example:
%% ```
%% place_lst() ->
%%   [coin_slot, cash_box, signal, storage, compartment].
%% '''
%% Here, we define the net to have the five places in the cookie vending
%% machine.
%%
%% <h4>trsn_lst/0</h4>
%%
%% The `trsn_lst/0' function lets us define the names of all transitions in the
%% net.
%%
%% Example:
%% ```
%% trsn_lst() ->
%%   [a, b].
%% '''
%% Here, we define the net to have the two places `a' and `b' in the cookie
%% vending machine.
%%
%% <h4>preset/1</h4>
%%
%% The `preset/1' lets us define the preset places of a given transition.
%%
%% Example:
%% ```
%% preset( a ) -> [coin_slot];
%% preset( b ) -> [signal, storage].
%% '''
%% Here, we define the preset of the transition `a' to be just the place
%% `coin_slot' while the transition `b' has the places `signal' and `storage'
%% in its preset.
%%
%% <h4>init_marking/2</h4>
%%
%% The `init_marking/2' function lets us define the initial marking for a given
%% place in the form of a token list. The argument `UsrInfo' is the user info
%% field that has been generated in the actor interface callback `init/1'.
%%
%% Example:
%% ```
%% init_marking( storage, _UsrInfo ) -> [cookie_box, cookie_box, cookie_box];
%% init_marking( _Place, _UsrInfo )  -> [].
%% '''
%% Here, we initialize the storage place with three `cookie_box' tokens. All
%% other places are left empty.
%%
%% <h4>is_enabled/3</h4>
%%
%% The `is_enabled/3' function is a predicate determining whether a given
%% transition is enabled in a given mode. The `UsrInfo' argument is the user
%% info field that has been created with `init/1'.
%%
%% Example:
%% ```
%% is_enabled( a, #{ coin_slot := [coin] }, _UsrInfo )                      -> true;
%% is_enabled( b, #{ signal := [sig], storage := [cookie_box] }, _UsrInfo ) -> true;
%% is_enabled( _Trsn, _Mode, _UsrInfo )                                     -> false.
%% '''
%% Here, we state that the transition `a' is enabled if it can consume a single
%% `coin' from the `coin_slot' place. Similarly, the transition `b' is enabled
%% if it can consume a `sig' token from the `signal' place and a `cookie_box'
%% token from the `storage` place. No other configuration can enable a
%% transition. E.g., managing to get a `button' token on the `coin_slot' place
%% will not enable any transition.
%%
%% <h4>fire/3</h4>
%%
%% The `fire/3' function defines what tokens are produced when a given
%% transition fires in a given mode. As arguments it takes the name of the
%% transition, and a firing mode in the form of a hash map mapping place names
%% to token lists. The `fire/3' function is called only on modes for which
%% `is_enabled/3' returns `true'. The `fire/3' function is expected to return
%% either a `{produce, ProduceMap}' tuple or the term `abort'. If `abort' is
%% returned, the firing is aborted. Nothing is produced or consumed.
%%
%% Example:
%% ```
%% fire( a, _Mode, _UsrInfo ) ->
%%   {produce, #{ cash_box => [coin], signal => [sig] }};
%% fire( b, _Mode, _UsrInfo ) ->
%%   {produce, #{ compartment => [cookie_box] }}.
%% '''
%% Here, the firing of the transition `a' produces a `coin' token on the
%% `cash_box' place and a `sig' token on the `signal' place. Similarly, the
%% firing of the transition `b' produces a `cookie_box' token on the
%% `compartment' place. We do not need to state the tokens to be consumed
%% because the firing mode already uniquely identifies the tokens to be
%% consumed.
%%
%%
%% <h3>Interface Callback Functions</h3>
%%
%% In addition to the structure callback functions there are another seven
%% callback functions that determine how the net instance appears as an Erlang
%% actor to the outside world:
%%
%% <ul>
%%   <li>`code_change/3' determines what happens when a hot code reload
%%       appears</li>
%%   <li>`handle_call/3' synchronous message exchange</li>
%%   <li>`handle_cast/2' asynchronous message reception</li>
%%   <li>`handle_info/2' asynchronous reception of an unformatted message</li>
%%   <li>`init/1' initializes the gen_pnet instance</li>
%%   <li>`terminate/2' determines what happens when the net instance is
%%       stopped</li>
%%   <li>`trigger/3' allows to add a side effects to the generation of a
%%       token</li>
%% </ul>
%%
%% <h4>code_change/3</h4>
%%
%% The `code_change/3' function determines what happens when a hot code reload
%% appears. This callback is identical to the `code_change/3' function in the
%% `gen_server' behavior.
%%
%% Example:
%% ```
%% code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.
%% '''
%%
%% <h4>handle_call/3</h4>
%%
%% The `handle_call/3' function performs a synchronous exchange of messages
%% between the caller and the net instance. The first argument is the request
%% message, the second argument is a tuple identifying the caller, and the third
%% argument is a `#net_state{}' record instance describing the current state of
%% the net. The `handle_call/3' function can generate a reply without changing
%% the net marking by returning a `{reply, Reply}' tuple, it can generate a
%% reply, consuming or producing tokens by returning a
%% `{reply, Reply, ConsumeMap, ProduceMap}' tuple, it can defer replying without
%% changing the net marking by returning `noreply', it can defer replying,
%% consuming or producing tokens by returning a
%% `{noreply, ConsumeMap, ProduceMap}' tuple, or it can stop the net instance by
%% returning `{stop, Reason, Reply}'.
%%
%% Example:
%% ```
%% handle_call( insert_coin, _From, _NetState ) ->
%%   {reply, ok, #{}, #{ coin_slot => [coin] }};
%%
%% handle_call( remove_cookie_box, _From, NetState ) ->
%%
%%   case gen_pnet:get_ls( compartment, NetState ) of
%%     []    -> {reply, {error, empty_compartment}};
%%     [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
%%   end;
%%
%% handle_call( _Request, _From, _NetState ) -> {reply, {error, bad_msg}}.
%% '''
%% Here, we react to two kinds of messages: Inserting a coin in the coin slot
%% and removing a cookie box from the compartment. Thus, we react to an
%% `insert_coin' message by replying with `ok', consuming nothing and producing
%% a `coin' token on the `coin_slot' place. When receiving a `remove_cookie_box'
%% message, we check whether the `compartment' place is empty, replying with an
%% error message if it is, otherwise replying with `ok', consuming one
%% `cookie_box' token from the `compartment' place, and producing nothing. Calls
%% that are neither `insert_coin' nor `remove_cookie_box' are responded to with
%% an error message.
%%
%% <h4>handle_cast/2</h4>
%%
%% The `handle_cast/2' function reacts to an asynchronous message received by
%% the net instance. The first argument is the request while the second argument
%% is a `#net_state{}' record instance. The `handle_cast/2' function can either
%% leave the net unchanged by returning `noreply' or it can consume or produce
%% tokens by returning a `{noreply, ConsumeMap, ProduceMap}' tuple.
%%
%% Example:
%% ```
%% handle_cast( _Request, _NetState ) -> noreply.
%% '''
%% Here, we just ignore any cast.
%%
%% <h4>handle_info/2</h4>
%%
%% The `handle_info/2' function reacts to an asynchronous, unformatted message
%% received by the net instance. The first argument is the message term while
%% the second argument is a `#net_state{}' record instance. The `handle_info/2'
%% function can either leave the net unchanged by returning `noreply' or it can
%% consume or produce tokens by returning a `{noreply, ConsumeMap, ProduceMap}'
%% tuple.
%%
%% Example:
%% ```
%% handle_info( _Request, _NetState ) -> noreply.
%% '''
%% Here, we just ignore any message.
%%
%% <h4>init/1</h4>
%%
%% The `init/1' function initializes the net instance. It is given an initial
%% argument which is provided with `gen_pnet:start_link/n'. The `init/1'
%% function is expected to return a user info field which is later handed to
%% other callback functions.
%%
%% Example:
%% ```
%% init( _NetArg ) -> [].
%% '''
%% Here, we return the empty list as a dummy user info field.
%%
%% <h4>terminate/2</h4>
%%
%% The `terminate/2' function determines what happens when the net instance is
%% stopped. The first argument is the reason for termination while the second
%% argument is a `#net_state{}' record instance. This callback is identical to
%% the `terminate/2' function in the `gen_server' behavior.
%%
%% Example:
%% ```
%% terminate( _Reason, _NetState ) -> ok.
%% '''
%%
%% <h4>trigger/3</h4>
%%
%% The `trigger/3' function determines what happens when a token is produced on
%% a given place. Its first argument `Place' is the place name, its second
%% argument `Token' is the token about to be produced, and its third argument
%% `NetState' is the current state of the net. The `trigger/3' function is
%% expected to return either `pass' in which case the token is produced
%% normally, or `drop' in which case the token is forgotten.
%%
%% Example:
%% ```
%% trigger( _Place, _Token, _NetState ) -> pass.
%% '''
%% Here, we simply let any token pass.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_pnet).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

% API functions
-export([start_link/3, start_link/4,
         ls/2,
         marking/1,
         call/2, call/3,
         cast/2,
         stats/1,
         reply/2,
         reset_stats/1,
         stop/1,
         usr_info/1,
         state_property/3]).

% Net state constructor and accessor functions
-export([get_ls/2, get_usr_info/1, get_stats/1]).

% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Type definitions
%%====================================================================

-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

-type handle_call_request() :: {ls, atom()} |
                               marking |
                               usr_info |
                               {call, _} |
                               stats |
                               reset_stats.

-type handle_call_result() :: {reply, _, #net_state{}} |
                              {noreply, #net_state{}} |
                              {stop, _, _, #net_state{}}.

-type handle_cast_request() :: continue |
                               {cast, _}.

-type handle_cast_result() :: {noreply, #net_state{}} |
                              {stop, _, #net_state{}}.

-type handle_info_result() :: {noreply, #net_state{}} |
                              {stop, _, #net_state{}}.

-type prop() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.

%%====================================================================
%% Callback definitions
%%====================================================================

%% Structure callbacks


-callback place_lst() -> [atom()].

-callback trsn_lst() -> [atom()].

-callback init_marking(Place :: atom(), UsrInfo :: _) -> [_].

-callback preset(Trsn :: atom()) -> [atom()].

-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              boolean().

-callback fire(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              abort | {produce, #{atom() => [_]}}.

%% Interface callbacks

-callback code_change(OldVsn :: _, NetState :: #net_state{}, Extra :: _) ->
              {ok, #net_state{}} | {error, _}.

-callback handle_call(Request :: _,
                      From :: {pid(), _},
                      NetState :: #net_state{}) ->
              {reply, _} |
              {reply, _, #{atom() => [_]}, #{atom() => [_]}} |
              noreply |
              {noreply, #{atom() => [_]}, #{atom() => [_]}} |
              {stop, _, _}.

-callback handle_cast(Request :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}, #{atom() => [_]}} |
              {stop, _}.

-callback handle_info(Info :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}, #{atom() => [_]}} |
              {stop, _}.

-callback init(NetArg :: _) -> _.

-callback terminate(Reason :: _, NetState :: #net_state{}) -> ok.

-callback trigger(Place :: atom(), Token :: _, NetState :: #net_state{}) ->
              pass | drop.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an unregistered net instance.
%% @see start_link/4
-spec start_link(NetMod, NetArg, Options) -> start_link_result()
              when NetMod :: atom(),
                   NetArg :: _,
                   Options :: [prop()].

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).


%% @doc Starts a net instance registered as `ServerName' using the callback
%%      module `NetMod' as the callback module for this net instance.
%%
%%      The `InitArg' argument is later handed to the `init/1' callback. The
%%      `ServerName' argument can be
%%      `{local, Name} | {global, Name} | {via, Module, ViaName}'. Internally,
%%      the server name `ServerName' and option list `Options' are handed down
%%      to `gen_server:start_link/4' as is.
%%
%% @see init/1


-spec start_link(ServerName, NetMod, InitArg, Options) -> start_link_result()
              when ServerName :: server_name(),
                   NetMod :: atom(),
                   InitArg :: _,
                   Options :: [prop()].

start_link(ServerName, NetMod, InitArg, Options)
  when is_tuple(ServerName), is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(ServerName, ?MODULE, {NetMod, InitArg}, Options).


%% @doc Query the list of tokens on the place named `Place' in the net instance
%%      identified as `Name'.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is either `{ok, [_]}' if the place exists or a
%%      `{error, #bad_place{}}' tuple.


-spec ls(Name, Place) -> {ok, [_]} | {error, #bad_place{}}
              when Name :: name(),
                   Place :: atom().

ls(Name, Place) when is_atom(Place) -> gen_server:call(Name, {ls, Place}).


%% @doc Query the marking map of the net instance identified as `Name'
%%      associating to each place name the list of tokens that this place holds.
%%
%%      Herein, `Name' can be a process id or a registered process name. The
%%      return value is the Petri net's marking map.


-spec marking(Name :: name()) -> #{atom() => [_]}.

marking(Name) -> gen_server:call(Name, marking).


%% @doc Query the user info term from the net instance identified as `Name'.


-spec usr_info(Name :: name()) -> _.

usr_info(Name) -> gen_server:call(Name, usr_info).


%% @doc Query the statistics gathered by the net instance identified as `Name'.
%%
%%      The throughput is given as a `#stats{}' record consisting of three
%%      `#stat{}' record instances characterizing the current, maximum, and
%%      minimum throughput of this net in transition firings per second.


-spec stats(Name :: name()) -> #stats{}.

stats(Name) -> gen_server:call(Name, stats).


%% @doc Requests the net instance identified as `Name' to clear its stats.


-spec reset_stats(Name :: name()) -> ok.

reset_stats(Name) -> gen_server:call(Name, reset_stats).


%% @doc Requests the net instance identified as `Name' to stop.


-spec stop(Name :: name()) -> ok.

stop(Name) -> gen_server:stop(Name).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.

%%      The timeout is implicitly set to five seconds.
%%
%% @see call/3


-spec call(Name :: name(), Request :: _) -> _.

call(Name, Request) -> gen_server:call(Name, {call, Request}).


%% @doc Synchronously send the term `Request' to the net instance identified as
%%      `Name' and return the reply.
%%
%%      The timeout is explicitly set to `Timeout'. The request is handled by
%%      the `handle_call/3' callback function of the interface module. Herein
%%      `Timeout' must be a non-negative integer or the atom `infinity'.


-spec call(Name, Request, Timeout) -> _
              when Name :: name(),
                   Request :: _,
                   Timeout :: non_neg_integer() | infinity.

call(Name, Request, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    gen_server:call(Name, {call, Request}, Timeout);

call(Name, Request, infinity) ->
    gen_server:call(Name, {call, Request}, infinity).


%% @doc Asynchronously send the term `Request' to the net instance identified as
%%      `Name'.
%%
%%      The request is handled by the `handle_cast/2' callback function of the
%%      interface module. Note that the cast succeeds even if a non-existing
%%      process is addressed or the net instance is down.


-spec cast(Name :: name(), Request :: _) -> ok.

cast(Name, Request) ->
    gen_server:cast(Name, {cast, Request}).


%% @doc Sends a reply to a calling client process.
%%
%%      This funciton is to be used when the reply to a caller has been
%%      deferred by returning `{noreply, _, _}' in `handle_call/3'.
%%
%% @see handle_call/3


-spec reply(Client, Reply) -> Result
              when Client :: {pid(), gen_server:reply_tag()},
                   Reply :: _,
                   Result :: ok.

reply(Client, Reply) when is_tuple(Client) ->
    gen_server:reply(Client, Reply).


%% @doc Checks if a predicate about the state of the net holds.
%%
%%      The function takes a Petri net instance identified as `Name' and asks it
%%      to verify the predicate `Pred' over its marking. Herein, `Pred' is a
%%      function that takes n token lists, where each of the token lists subsume
%%      the tokens present on the places identified by the `PlaceLst' argument.
%%      The predicate is expected to return either `ok' or `{error, Reason}'
%%      where Reason can be any Erlang term.


-spec state_property(Name, Pred, PlaceLst) -> ok | {error, Reason}
              when Name :: name(),
                   Pred :: fun((...) -> ok | {error, Reason}),
                   PlaceLst :: [atom()].

state_property(Name, Pred, PlaceLst)
  when is_list(PlaceLst),
       is_function(Pred, length(PlaceLst)) ->

    Marking = gen_pnet:marking(Name),
    ArgLst = [ maps:get(Place, Marking) || Place <- PlaceLst ],
    apply(Pred, ArgLst).


%%====================================================================
%% Net state constructor and accessor functions
%%====================================================================


%% @doc Extracts the list of tokens on a given place from a given net state.
%%
%%      Throws an error if the list does not exist.
-spec get_ls(Place :: atom(), NetState :: #net_state{}) -> [_].

get_ls(Place, #net_state{marking = Marking}) -> maps:get(Place, Marking).


%% @doc Extracts the user info field from a given net state.
-spec get_usr_info(NetState :: #net_state{}) -> _.

get_usr_info(#net_state{usr_info = UsrInfo}) -> UsrInfo.


%% @doc Extracts the stats field from a given net instance.
-spec get_stats(NetState :: #net_state{}) -> #stats{}.

get_stats(#net_state{stats = Stats}) -> Stats.


%%====================================================================
%% Generic server callback functions
%%====================================================================


%% @private
-spec code_change(OldVsn, NetState, Extra) -> {ok, #net_state{}} | {error, _}
              when OldVsn :: _,
                   NetState :: #net_state{},
                   Extra :: _.

code_change(OldVsn, NetState = #net_state{net_mod = NetMod}, Extra) ->
    NetMod:code_change(OldVsn, NetState, Extra).


%% @private
-spec handle_call(Request, From, NetState) -> handle_call_result()
              when Request :: handle_call_request(),
                   From :: {pid(), _},
                   NetState :: #net_state{}.

handle_call({ls, Place}, _From, NetState = #net_state{marking = Marking}) ->

    Reply = case maps:is_key(Place, Marking) of
                true -> {ok, maps:get(Place, Marking)};
                false -> {error, #bad_place{name = Place}}
            end,

    {reply, Reply, NetState};

handle_call(marking, _From, NetState = #net_state{marking = Marking}) ->
    {reply, Marking, NetState};

handle_call(usr_info, _From, NetState = #net_state{usr_info = UsrInfo}) ->
    {reply, UsrInfo, NetState};

handle_call({call, Request},
            From,
            NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_call(Request, From, NetState) of

        {reply, Reply} ->
            {reply, Reply, NetState};

        {reply, Reply, CnsMap, ProdMap} ->
            NetState1 = cns(CnsMap, NetState),
            NetState2 = handle_trigger(ProdMap, NetState1),
            continue(self()),
            {reply, Reply, NetState2};

        noreply ->
            {noreply, NetState};

        {noreply, CnsMap, ProdMap} ->
            NetState1 = cns(CnsMap, NetState),
            NetState2 = handle_trigger(ProdMap, NetState1),
            continue(self()),
            {noreply, NetState2};

        {stop, Reason, Reply} ->
            {stop, Reason, Reply, NetState}

    end;

handle_call(stats, _From, NetState = #net_state{stats = Stats}) ->
    {reply, Stats, NetState};

handle_call(reset_stats, _From, NetState) ->
    {reply, ok, NetState#net_state{stats = undefined}}.


%% @private
-spec handle_cast(Request, NetState) -> handle_cast_result()
              when Request :: handle_cast_request(),
                   NetState :: #net_state{}.

handle_cast(continue,
            NetState = #net_state{
                         stats = Stats,
                         tstart = T1,
                         cnt = Cnt
                        }) ->

    case progress(NetState) of

        abort ->
            {noreply, NetState};

        {delta, Mode, Pm} ->

            NetState1 = cns(Mode, NetState),
            NetState2 = handle_trigger(Pm, NetState1),
            continue(self()),

            NetState3 = if
                            Cnt < 1000 -> NetState2#net_state{cnt = Cnt + 1};
                            true ->

                                T2 = os:system_time(),
                                Tmean = round((T1 + T2) / 2),
                                Tdelta = T2 - T1,
                                CurrentFps = 1000000000000 / Tdelta,

                                Current = #stat{t = Tmean, fps = CurrentFps},

                                {Hi1, Lo1} = case Stats of
                                                 undefined -> {Current, Current};
                                                 #stats{hi = H, lo = L} -> {H, L}
                                             end,

                                #stat{fps = HiFps} = Hi1,
                                #stat{fps = LoFps} = Lo1,

                                Hi2 = if
                                          CurrentFps > HiFps -> Current;
                                          true -> Hi1
                                      end,

                                Lo2 = if
                                          CurrentFps < LoFps -> Current;
                                          true -> Lo1
                                      end,

                                NetState2#net_state{
                                  stats = #stats{
                                            current = Current,
                                            hi = Hi2,
                                            lo = Lo2
                                           },
                                  tstart = T2,
                                  cnt = 0
                                 }
                        end,

            {noreply, NetState3}

    end;

handle_cast({cast, Request}, NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_cast(Request, NetState) of

        noreply ->
            {noreply, NetState};

        {noreply, CnsMap, ProdMap} ->
            NetState1 = cns(CnsMap, NetState),
            NetState2 = handle_trigger(ProdMap, NetState1),
            continue(self()),
            {noreply, NetState2};

        {stop, Reason} ->
            {stop, Reason, NetState}

    end.


%% @private
-spec handle_info(Info, NetState) -> handle_info_result()
              when Info :: _,
                   NetState :: #net_state{}.

handle_info(Info, NetState = #net_state{net_mod = NetMod}) ->

    case NetMod:handle_info(Info, NetState) of

        noreply ->
            {noreply, NetState};

        {noreply, CnsMap, ProdMap} ->
            NetState1 = cns(CnsMap, NetState),
            NetState2 = handle_trigger(ProdMap, NetState1),
            continue(self()),
            {noreply, NetState2};

        {stop, Reason} ->
            {stop, Reason, NetState}

    end.


%% @private
-spec init(ArgPair :: {atom(), _}) -> {ok, #net_state{}}.

init({NetMod, NetArg}) ->

    UsrInfo = NetMod:init(NetArg),

    PlaceLst = NetMod:place_lst(),

    F = fun(P, Acc) ->
                Acc#{P => NetMod:init_marking(P, UsrInfo)}
        end,

    InitMarking = lists:foldl(F, #{}, PlaceLst),

    continue(self()),

    {ok, #net_state{
           net_mod = NetMod,
           usr_info = UsrInfo,
           marking = InitMarking,
           stats = undefined,
           tstart = os:system_time(),
           cnt = 0
          }}.


%% @private
-spec terminate(Reason :: _, NetState :: #net_state{}) -> ok.

terminate(Reason, NetState = #net_state{net_mod = NetMod}) ->
    NetMod:terminate(Reason, NetState).


%%====================================================================
%% Internal functions
%%====================================================================


%% @doc Continue making progress in net instance under process id `Name'.
%%
%%      Note that continuing succeeds even if a non-existing process is
%%      addressed or the net instance is down.
-spec continue(Name :: pid()) -> ok.

continue(Name) ->
    gen_server:cast(Name, continue).


-spec handle_trigger(ProdMap, NetState) -> #net_state{}
              when ProdMap :: #{atom() => [_]},
                   NetState :: #net_state{}.

handle_trigger(ProdMap, NetState = #net_state{net_mod = NetMod}) ->

    G = fun(P, TkLst, Acc) ->

                F = fun(Tk, A) ->
                            case NetMod:trigger(P, Tk, NetState) of
                                pass -> [Tk | A];
                                drop -> A
                            end
                    end,

                TkLst1 = lists:foldl(F, [], TkLst),
                Acc#{P => TkLst1}

        end,

    ProdMap1 = maps:fold(G, #{}, ProdMap),
    prd(ProdMap1, NetState).


-spec cns(Mode, NetState) -> #net_state{}
              when Mode :: #{atom() => [_]},
                   NetState :: #net_state{}.

cns(Mode, NetState = #net_state{marking = Marking}) ->

    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst -- maps:get(T, Mode, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


-spec prd(ProdMap, NetState) -> #net_state{}
              when ProdMap :: #{atom() => [_]},
                   NetState :: #net_state{}.

prd(ProdMap, NetState = #net_state{marking = Marking}) ->

    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst ++ maps:get(T, ProdMap, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


-spec progress(NetState :: #net_state{}) ->
          abort | {delta, #{atom() => [_]}, #{atom() => [_]}}.

progress(#net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }) ->

    % get all transitions in the net
    TrsnLst = NetMod:trsn_lst(),

    F = fun(T, Acc) ->
                Preset = NetMod:preset(T),
                MLst = enum_mode(Preset, Marking),
                IsEnabled = fun(M) -> NetMod:is_enabled(T, M, UsrInfo) end,
                EnabledMLst = lists:filter(IsEnabled, MLst),
                case EnabledMLst of
                    [] -> Acc;
                    [_ | _] -> Acc#{T => EnabledMLst}
                end
        end,

    % derive a map listing all enabled modes for each transition
    ModeMap = lists:foldl(F, #{}, TrsnLst),

    % delegate enabled mode map to attempt_progress function
    attempt_progress(ModeMap, NetMod, UsrInfo).


-spec attempt_progress(ModeMap, NetMod, UsrInfo) -> abort | {delta, _, _}
              when ModeMap :: #{atom() => [_]},
                   NetMod :: atom(),
                   UsrInfo :: _.

attempt_progress(ModeMap, NetMod, UsrInfo) ->

    case maps:size(ModeMap) of

        0 -> abort;
        _ ->

            TrsnLst = maps:keys(ModeMap),
            Trsn = lib_combin:pick_from(TrsnLst),
            #{Trsn := ModeLst} = ModeMap,
            Mode = lib_combin:pick_from(ModeLst),

            case NetMod:fire(Trsn, Mode, UsrInfo) of

                {produce, ProdMap} ->
                    {delta, Mode, ProdMap};

                abort ->
                    ModeLst1 = ModeLst -- [Mode],
                    case ModeLst1 of
                        [] ->
                            attempt_progress(maps:remove(Trsn, ModeMap), NetMod, UsrInfo);
                        [_ | _] ->
                            attempt_progress(ModeMap#{Trsn := ModeLst1}, NetMod, UsrInfo)
                    end

            end
    end.


-spec enum_mode(Preset, Marking) -> [#{atom() => [_]}]
              when Preset :: [atom()],
                   Marking :: #{atom() => [_]}.

enum_mode(Preset, Marking) ->

    F = fun(P, Acc) ->
                N = maps:get(P, Acc, 0),
                Acc#{P => N + 1}
        end,

    % gather count map
    CountMap = lists:foldl(F, #{}, Preset),

    G = fun(P, N, Acc) ->
                #{P := TkLst} = Marking,
                Acc#{P => lib_combin:cnr(N, TkLst)}
        end,

    % enumerate drawing combinations for each preset place individually
    CmbMap = maps:fold(G, #{}, CountMap),

    % enumerate permutations of map containing drawing combinations
    lib_combin:permut_map(CmbMap).
