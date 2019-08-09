module Domain.Utils.String exposing (isAllLetters)


isAllLetters : String -> Bool
isAllLetters string =
    let
        isLetter c =
            List.member c letters

        letters =
            String.toList "abcdefghijklmnopqrstuvwxyzăîâțș"
    in
    string |> String.toLower |> String.all isLetter
