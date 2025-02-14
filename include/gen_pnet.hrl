%% -*- erlang -*-
%%
%% A generic Petri net OTP behavior.
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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>

%%====================================================================
%% Record definitions
%%====================================================================

-record(bad_place, {name}).
-record(net_state, {marking, net_mod, usr_info, stats, tstart, cnt}).
-record(stat, {t, fps}).
-record(stats, {current, hi, lo}).
