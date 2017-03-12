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

-module( trsn_sup ).
-behavior( supervisor ).

-export( [init/1] ).
-export( [start_link_trsn/2] ).

%%====================================================================
%% Supervisor callback functions
%%====================================================================

init( NetMod ) ->

  TrsnLst = NetMod:trsn_lst(),

  ChildSpecs = [create_child_spec( Trsn, NetMod ) || Trsn <- TrsnLst],

  SupFlags = #{
    strategy  => one_for_one,
    intensity => 10,          % restart 10 times
    period    => 5            % in 5 seconds
  },

  {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% API functions
%%====================================================================

start_link_trsn( _Trsn, _NetMod ) ->
  Pid = spawn_link( fun() -> timer:sleep( 10000 ) end ),
  {ok, Pid}.

%%====================================================================
%% Internal functions
%%====================================================================


create_child_spec( Trsn, NetMod ) ->

  ChildId = list_to_atom(
               string:join( [atom_to_list( NetMod ),
                             atom_to_list( Trsn )], "_" ) ),

  io:format( "child id: ~p~n", [ChildId] ),

  Start = {trsn_sup, start_link_trsn, [Trsn, NetMod]},

  io:format( "start: ~p~n", [Start] ),

  #{ id       => ChildId,
     start    => Start,
     restart  => permanent,
     shutdown => brutal_kill,
     type     => worker
  }.

