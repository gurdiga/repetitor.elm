module Domain.Utils.Name exposing (allValidSymbols)


allValidSymbols : String -> Bool
allValidSymbols string =
    let
        isValidSymbol c =
            List.member c validSymbols

        validSymbols =
            String.toList "abcdefghijklmnopqrstuvwxyzăîâțţșş -"
    in
    string |> String.toLower |> String.all isValidSymbol
