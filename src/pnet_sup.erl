%%%-------------------------------------------------------------------
%% @doc pnet top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module( pnet_sup ).

-behaviour( supervisor ).

%% API
-export( [start_link/0] ).

%% Supervisor callbacks
-export( [init/1] ).

-define( SERVER, ?MODULE ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link( {local, ?SERVER}, ?MODULE, [] ).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init( [] ) ->

  Pnmon = #{ id       => pnmon,
             start    => {pnmon, start_link, []},
             restart  => permanent,
             shutdown => 5000,
             type     => worker,
             modules  => [pnmon]
           },

  TrsnSup = #{ id       => trsn_sup,
               start    => {trsn_sup, start_link, []},
               restart  => permanent,
               shutdown => 5000,
               type     => supervisor,
               modules  => [trsn_sup]
             },

  {ok, {{one_for_all, 3, 10}, [Pnmon, TrsnSup]}}.

%%====================================================================
%% Internal functions
%%====================================================================
