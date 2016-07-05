-module( cvm ).

-export( [preset/1, enum_consum_lst/3, fire/2, init/1, place_lst/0,
          trsn_lst/0] ).

-behaviour( gen_pnet ).

-include( "include/gen_pnet.hrl" ).

init( _UserArg ) -> {ok, []}.

place_lst() -> [coin_slot, compartment].

trsn_lst() -> [t].

preset( t ) -> [coin_slot].

enum_consum_lst( t, #{ coin_slot := CoinLst }, _UserInfo ) ->
  [[C] || C = #token{ place = coin_slot } <- CoinLst].

fire( [#token{ place = coin_slot }], _UserInfo ) ->
  [#token{ place = compartment }].
