-module(trivial_test).
-include_lib("eunit/include/eunit.hrl").


trivial_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_set/1}.


setup() ->
    {ok, Pid} = gen_pnet:start_link(trivial_net, [], []),
    Pid.


cleanup(Pid) ->
    ok = gen_pnet:stop(Pid).


test_set(Pid) ->
    [marking_test(Pid),
     call_test(Pid)].


marking_test(Pid) ->
    {"trivial marking test",
     fun() ->
             Marking = gen_pnet:marking(Pid),
             ?assertEqual(#{}, Marking)
     end}.


call_test(Pid) ->
    {"trivial call test",
     fun() ->
             ?assertEqual({error, bad_request}, gen_pnet:call(Pid, {request}))
     end}.
