%% -*- erlang -*-
%%
%% A generic Petri net OTP library.
%%
%% Copyright 2016 Jorgen Brandt. All Rights Reserved.
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
%% @author Jorgen Brandt <joergen.brandt@onlinehome.de>



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

-callback trigger( Place :: atom(), Token :: _ ) -> pass | consume.

