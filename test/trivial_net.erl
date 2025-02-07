-module(trivial_net).

-behaviour(gen_pnet).

-export([place_lst/0,
	 trsn_lst/0,
	 init_marking/2,
	 preset/1,
	 is_enabled/3,
	 fire/3,
	 trigger/3]).

-export([code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 init/1,
	 terminate/2]).


%% Structure callbacks

place_lst() ->
    [].

trsn_lst() ->
    [].

init_marking(_Place, _UsrInfo) ->
    [].

preset(_Trsn) ->
    [].

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.



%% Interface callbacks

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_request}}.

handle_cast(_Request, _NetState) ->
    noreply.

handle_info(_Info, _NetState) ->
    noreply.

init(_NetArg) ->
    [].

terminate(_Reason, _NetState) ->
    ok.

trigger(_Place, _Token, _NetState) ->
    pass.
