-module( cvm1 ).

-behaviour( gen_pnet ).

-export( [preset/1, enum_consum_lst/3, fire/2, init/1, place_lst/0,
          trsn_lst/0] ).

-include( "include/gen_pnet.hrl" ).

%%====================================================================
%% gen_pnet callback functions
%%====================================================================

place_lst() -> [coin_slot, compartment].

trsn_lst() -> [t].

preset( t ) -> [coin_slot].

init( _UserArg ) -> {ok, []}.

enum_consum_lst( t, #{ coin_slot := CoinLst }, _UserInfo ) ->
  [[C] || C <- CoinLst].

fire( [#token{ place = coin_slot }], _UserInfo ) ->
  [#token{ place = compartment }].
