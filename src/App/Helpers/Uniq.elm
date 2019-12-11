module App.Helpers.Uniq exposing (uniq)


uniq : List String -> List String
uniq list =
    List.foldr
        (\curr result ->
            if List.member (String.toLower curr) (List.map String.toLower result) then
                result

            else
                curr :: result
        )
        []
        list
