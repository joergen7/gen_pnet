-module( cvm1 ).

-export( [init/0, hot_transition_lst/0, place_get/2, consume_map_lst/2,
          place_add/3, place_remove/3, produce_map/2] ).

-behaviour( pnet ).

init() -> #{ coin_slot => [coin], compartment => [] }.

hot_transition_lst() -> [t].

place_add( coin_slot, [coin], UserState = #{ coin_slot := CoinLst } ) ->
  UserState#{ coin_slot => [coin|CoinLst] };

place_add( compartment, [box], UserState = #{ compartment := BoxLst } ) ->
  UserState#{ compartment => [box|BoxLst] }.

place_remove( coin_slot, [coin], UserState = #{ coin_slot := CoinLst } ) ->
  UserState#{ coin_slot => CoinLst--[coin] };

place_remove( compartment, [box], UserState = #{ compartment := BoxLst } ) ->
  UserState#{ compartment => BoxLst--[box] }.

place_get( Place, UserState ) -> maps:get( Place, UserState ).

consume_map_lst( t, #{ coin_slot := CoinLst } ) ->
  [#{ coin_slot => [coin] } || coin <- CoinLst].

produce_map( t, #{ coin_slot := [coin] } ) ->
  #{ compartment => [box] }.