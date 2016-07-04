-module( cvm ).

-export( [place_lst/0, subscr_map/0] ).

-behaviour( pnet ).

place_lst() -> [coin_slot, compartment].
subscr_map() -> #{t => [coin_slot]}.
