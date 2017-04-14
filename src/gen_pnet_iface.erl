%% -*- erlang -*-
%%
%% A generic Petri net OTP behavior.
%%
%% Copyright 2016-2017 Jorgen Brandt.
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
%% @author Jorgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.1
%% @copyright 2016-2017 Jorgen Brandt.
%% @doc Callback function definitions for Petri net actor interface.
%%
%% In addition to the structure callback functions there are another six
%% callback functions that determine how the net instance appears as an Erlang
%% actor to the outside world:
%%
%% <ul>
%%   <li>`code_change/3' determines what happens when a hot code reload
%%       appears</li>
%%   <li>`handle_call/3' synchronous message exchange</li>
%%   <li>`handle_cast/2' asynchronous message reception</li>
%%   <li>`handle_info/2' asynchronous reception of an unformatted message</li>
%%   <li>`terminate/2' determines what happens when the net instance is
%%       stopped</li>
%%   <li>`trigger/2' allows to add a side effects to the generation of a
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
%% the net. The `handle_call/3' function can either generate a reply without
%% changing the net marking by returning a `{reply, Reply}' tuple or it can
%% generate a reply, consuming or producing tokens by returning a
%% `{reply, Reply, ConsumeMap, ProduceMap}' tuple.
%%
%% Example:
%% ```
%% handle_call( insert_coin, _, _ ) ->
%%   {reply, ok, #{}, #{ coin_slot => [coin] }};
%%
%% handle_call( remove_cookie_box, _,
%%              #net_state{ marking = #{ compartment := C } } ) ->
%%
%%   case C of
%%     []    -> {reply, {error, empty_compartment}};
%%     [_|_] -> {reply, ok, #{ compartment => [cookie_box] }, #{}}
%%   end;
%%
%% handle_call( _, _, _ ) -> {reply, {error, bad_msg}}.
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
%% <h4>trigger/2</h4>
%%
%% The `trigger/2' function determines what happens when a token is produced on
%% a given place. Its first argument is the place name and its second argument
%% is the token about to be produced. The `trigger/2' function is expected to
%% return either `pass' in which case the token is produced normally, or `drop'
%% in which case the token is forgotten.
%%
%% Example:
%% ```
%% trigger( _, _ ) -> pass.
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
            {reply, _} | {reply, _, #{ atom() => [_] }, #{ atom() => [_] }}.

-callback handle_cast( Request :: _, NetState :: #net_state{} ) ->
            noreply | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}.

-callback handle_info( Info :: _, NetState :: #net_state{} ) ->
            noreply | {noreply, #{ atom() => [_] }, #{ atom() => [_] }}.

-callback terminate( Reason :: _, NetState :: #net_state{} ) -> ok.

-callback trigger( Place :: atom(), Token :: _ ) -> pass | drop.

