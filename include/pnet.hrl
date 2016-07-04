%%====================================================================
%% Type definitions
%%====================================================================



%%====================================================================
%% Record definitions
%%====================================================================


-record( token, { place, info = [] } ).
-record( lock,  { trsn_label, token } ).

-record( pnet_init_event, { pid, mod } ).
-record( pnet_terminate_event, { pid, mod, reason } ).
-record( trsn_init_event, { pid, mod, pnet_pid, label } ).
-record( trsn_terminate_event, { pid, mod, pnet_pid, label, reason } ).
-record( token_recv_event, { pid, mod, token } ).
-record( get_lock_event, {} ).
-record( rel_lock_event, {} ).
-record( lock_na_event, {} ).
-record( trsn_down_event, {} ).