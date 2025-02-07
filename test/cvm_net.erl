-module(cvm_net).

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
    ['CoinSlot', 'Send', 'CashBox', 'Storage', 'Recv', 'Compartment'].

trsn_lst() ->
    [send, recv].

init_marking('Storage', _UsrInfo) -> io:fwrite("some\n"), [cookie, cookie, cookie];
init_marking(_Place, _UsrInfo)    -> io:fwrite("none\n"), [].

preset(send)  -> ['CoinSlot'];
preset(recv)  -> ['Storage', 'Send'].

is_enabled(_Trsn, _Mode, _UsrInfo) ->
    true.

fire(send, _Mode, _UsrInfo) -> {produce, #{'Send' => [t]}};
fire(recv, _Mode, _UsrInfo) -> {produce, #{'Compartment' => [cookie]}}.




%% Interface callbacks

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

handle_call(insert_coin, _From, _NetState) -> {reply, ok, #{'CoinSlot' => [coin]}};
handle_call(_Request, _From, _NetState)    -> {reply, {error, bad_request}}.

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
