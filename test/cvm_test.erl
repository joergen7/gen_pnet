-module(cvm_test).
-include_lib("eunit/include/eunit.hrl").


trivial_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_set/1}.


setup() ->
    {ok, Pid} = gen_pnet:start_link(cvm_net, [], []),
    Pid.


cleanup(Pid) ->
    ok = gen_pnet:stop(Pid).


test_set(Pid) ->
    [marking_test(Pid),
     get_cookie_test(Pid)].


marking_test(Pid) ->
    {"cvm marking test",
     fun() ->
             Marking = gen_pnet:marking(Pid),
             #{'Storage' := StorageMarking} = Marking,
             ?assertEqual([cookie, cookie, cookie], StorageMarking)
     end}.


get_cookie_test(Pid) ->
    {"get cookie",
     fun() ->
             gen_pnet:call(Pid, insert_coin),
             Marking = gen_pnet:marking(Pid),
             #{'Storage' := StorageMarking, 'Compartment' := CompartmentMarking} = Marking,
             ?assertEqual([cookie, cookie], StorageMarking),
             ?assertEqual([cookie], CompartmentMarking)
     end}.
