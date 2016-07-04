%%====================================================================
%% Type definitions
%%====================================================================



%%====================================================================
%% Record definitions
%%====================================================================

%% DEPRECATED
-record( cold_transition, {src_place, pnet_pid, dest_place} ).
-record( add_token, {place, token} ).
%% ----------

-record( token, {place, id, info} ).
-record( lock,  {token, transition} ).

-record( pnet_init_event, {pid, mod} ).
-record( pnet_terminate_event, {pid, mod, reason} ).
-record( trsn_init_event, {pid, pnet_pid, label} ).
-record( trsn_terminate_event, {pid, pnet_pid, label, reason} ).
-record( get_lock_event, {} ).
-record( rel_lock_event, {} ).
-record( lock_na_event, {} ).
-record( trsn_down_event, {} ).