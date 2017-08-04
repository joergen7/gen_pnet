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
%% @version 0.1.5
%% @copyright 2016-2017 Jörgen Brandt
%% @doc Callback function definitions for Petri net actor interface.
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
%% The `init/1' function initializes the net instance. It is given a start
%% argument term which is the start argument term that was provided with
%% `gen_pnet:start_link/n'. As a return value a tuple of the form
%% `{ok, #net_state{}}' is expected. We can construct it with the help of
%% `gen_pnet:new/2'. This function takes two arguments: The module implementing
%% the net structure callback functions as well as a user info field.
%%
%% Example:
%% ```
%% init( _Args ) -> {ok, gen_pnet:new( ?MODULE, [] )}.
%% '''
%% Here, we instantiate a `#net_state{}' record denoting the current module as
%% the Petri net structure callback module and the empty list as the user-info
%% field.
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

-module( gen_pnet_iface ).

-include( "gen_pnet.hrl" ).

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

