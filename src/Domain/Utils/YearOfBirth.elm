module Domain.Utils.YearOfBirth exposing (YearOfBirth, makeYearOfBirth)


type YearOfBirth
    = YearOfBirth Int


makeYearOfBirth : String -> Result String YearOfBirth
makeYearOfBirth year =
    String.toInt year
        |> Maybe.map YearOfBirth
        |> Result.fromMaybe ("An de naștere invalid: " ++ year)
