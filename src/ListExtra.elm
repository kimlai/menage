module ListExtra exposing (..)


unique : List a -> List a
unique list =
    List.foldl
        (\element result ->
            if List.member element result then
                result

            else
                result ++ [ element ]
        )
        []
        list


dropAtIndex : Int -> List a -> List a
dropAtIndex index list =
    List.take index list ++ List.drop (index + 1) list
