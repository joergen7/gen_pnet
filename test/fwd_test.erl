-module(fwd_test).
-include_lib("eunit/include/eunit.hrl").


trivial_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_set/1}.


setup() ->
    {ok, Pid} = gen_pnet:start_link(fwd_net, [], []),
    Pid.


cleanup(Pid) ->
    ok = gen_pnet:stop(Pid).


test_set(Pid) ->
    [marking_test(Pid)].


marking_test(Pid) ->
    {"fwd marking test",
     fun() ->
             Marking = gen_pnet:marking(Pid),
             #{
               'A' := AMarking,
               'B' := BMarking
              } = Marking,
             ?assertEqual([], AMarking),
             ?assertEqual([x, x], BMarking)
     end}.
