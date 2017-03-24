-module( lib_pnet ).

-export( [enum_mod/3] ).

-record( ebadplace, { name } ).
-record( eplaceempty, { name } ).

-spec enum_mod( PlaceLst, MarkingMap, Acc ) -> Result
when PlaceLst   :: [atom()],
     MarkingMap :: #{ atom() => [_] },
     Acc        :: [#{ atom() => [_] }],
     Result     :: {ok, [#{ atom() => [_] }]}
                 | {error, #ebadplace{}}
                 | {error, #eplaceempty{}}.

enum_mod( [], _, Acc ) -> return( Acc );

enum_mod( [Place|Tl], MarkingMap, [] ) ->

  case maps:is_key( Place, MarkingMap ) of
    false -> raise( #ebadplace{ name = Place } );
    true  ->

      #{ Place := TokenLst } = MarkingMap,

      case TokenLst of
        []    -> raise( #eplaceempty{ name = Place } );
        [_|_] -> 

          Acc1 = [#{ Place => [T] } || T <- lists:usort( TokenLst )],
          MarkingMap1 = MarkingMap#{ Place => TokenLst--[T] },

          enum_mod( Tl, MarkingMap1, Acc1 )

      end
  end;

enum_mod( [Place|Tl], MarkingMap, Acc ) ->

  case maps:is_key( Place, MarkingMap ) of
    false -> raise( #ebadplace{ name = Place } );
    true  ->

      #{ Place := TokenLst } = MarkingMap,

      case TokenLst of
        []    -> raise( #eplaceempty{ name = Place } );
        [_|_] -> 

          PreviousLst = maps:get( Place, Acc, [] ),

          Acc1 = [#{ Place => [T|PreviousLst] } || T <- lists:usort( TokenLst )],
          MarkingMap1 = MarkingMap#{ Place => TokenLst--[T] },

          enum_mod( Tl, MarkingMap1, Acc1 )

      end
  end.





raise( Reason ) ->
  {error, Reason}.

return( Result ) ->
  {ok, Result}.