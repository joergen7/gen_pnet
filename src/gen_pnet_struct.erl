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
%% @doc Callback function definitions for Petri net structure definition.
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
%% @end
%% -------------------------------------------------------------------


-module( gen_pnet_struct ).

-callback place_lst() -> [atom()].

-callback trsn_lst() -> [atom()].

-callback init_marking( Place :: atom(), UsrInfo :: _ ) -> [_].

-callback preset( Trsn :: atom() ) -> [atom()].

-callback is_enabled( Trsn :: atom(), Mode :: #{ atom() => [_]}, UsrInfo :: _ ) ->
            boolean().

-callback fire( Trsn :: atom(), Mode :: #{ atom() => [_] }, UsrInfo :: _ ) ->
            abort | {produce, #{ atom() => [_] }}.
