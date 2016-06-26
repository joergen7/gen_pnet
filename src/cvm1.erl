-module( cvm1 ).

-export( [init/0, hot_transition_lst/0, place_get/2, consume/2,
          place_add/3, place_remove/3, produce/2] ).

-behaviour( pnet ).

init() -> {ok, #{ coin_slot => [coin], compartment => [] }}.

hot_transition_lst() -> [t].

place_add( coin_slot, [coin], UserState = #{ coin_slot := CoinLst } ) ->
  {ok, UserState#{ coin_slot => [coin|CoinLst] }};

place_add( compartment, [box], UserState = #{ compartment := BoxLst } ) ->
  {ok, UserState#{ compartment => [box|BoxLst] }}.

place_remove( coin_slot, [coin], UserState = #{ coin_slot := CoinLst } ) ->
  {ok, UserState#{ coin_slot => CoinLst--[coin] }};

place_remove( compartment, [box], UserState = #{ compartment := BoxLst } ) ->
  {ok, UserState#{ compartment => BoxLst--[box] }}.

place_get( Place, UserState ) -> maps:get( Place, UserState ).

consume( t, #{ coin_slot := CoinLst } ) ->
  [#{ coin_slot => [coin] } || coin <- CoinLst].

produce( t, #{ coin_slot := [coin] } ) ->
  #{ compartment => [box] }.