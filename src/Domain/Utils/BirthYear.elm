module Domain.Utils.BirthYear exposing (BirthYear, birthYearToString, makeBirthYear)


type BirthYear
    = BirthYear Int


makeBirthYear : String -> Result String BirthYear
makeBirthYear string =
    let
        trimedString =
            String.trim string

        int =
            String.toInt trimedString
                |> Maybe.map BirthYear
                |> Result.fromMaybe ("An de naștere invalid: " ++ trimedString)

        saneInt =
            case int of
                Err error ->
                    Err error

                Ok (BirthYear year) ->
                    case reasonToReject year of
                        Nothing ->
                            Ok (BirthYear year)

                        Just reason ->
                            Err reason

        result =
            saneInt
    in
    result


reasonToReject : Int -> Maybe String
reasonToReject int =
    if int <= 0 then
        Just "Incorrect number."

    else if int > currentYear - 7 then
        Just "You are probably too young"

    else if int < currentYear - 100 then
        Just "You are probably too old"

    else
        Nothing


birthYearToString : BirthYear -> String
birthYearToString (BirthYear int) =
    String.fromInt int


currentYear : Int
currentYear =
    2019



-- TODO: Get it from the current time somehow?
