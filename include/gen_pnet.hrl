%%====================================================================
%% Record definitions
%%====================================================================

-record( bad_place, {name} ).
-record( net_state, { marking = #{}, net_mod, iface_mod, stats, tstart, cnt } ).
-record( stat, {t, fps} ).
-record( stats, {current, hi, lo} ).
