%% -*- erlang -*-
%%
%% %CopyrightBegin%
%%
%% A generic Petri net OTP library
%%
%% Copyright 2016 Jörgen Brandt. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% @author Jörgen Brandt <brandjoe@hu-berlin.de>

-module( gen_pnet_net ).

-callback place_lst() ->
            [atom()].

-callback trsn_lst() ->
            [atom()].

-callback init_marking() ->
            pass | {produce, #{ atom() => [_] }}.

-callback preset( Trsn :: atom() ) ->
            [atom()].

-callback mode_lst( Trsn :: atom(), RelevantMap :: #{ atom() => [_] } ) ->
            [#{ atom() => [_] }].

-callback fire( Trsn :: atom(), ConsumeMap :: #{ atom() => [_] } ) ->
            pass | {produce, ProduceMap :: #{ atom() => [_] }}.


