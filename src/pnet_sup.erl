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
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init( [] ) ->

    Pnmon = #{id       => ?SERVER,
              start    => {pnmon, start_link, []},
              restart  => permanent,
              shutdown => 5000,
              type     => worker,
              modules  => [pnmon]},

    {ok, { {one_for_one, 3, 10}, [Pnmon]} }.

%%====================================================================
%% Internal functions
%%====================================================================
