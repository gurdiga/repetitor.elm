module Domain.Utils.BirthYear exposing (BirthYear, birthYearToString, makeBirthYear)


type BirthYear
    = BirthYear Int


makeBirthYear : Int -> String -> Result String BirthYear
makeBirthYear currentYear string =
    let
        trimedString =
            String.trim string

        int =
            String.toInt trimedString
                |> Maybe.map BirthYear
                |> Result.fromMaybe ("Anul nașterii este incorect: „" ++ trimedString ++ "”")

        saneInt =
            case int of
                Err error ->
                    Err error

                Ok (BirthYear year) ->
                    case reasonToReject year currentYear of
                        Nothing ->
                            Ok (BirthYear year)

                        Just reason ->
                            Err reason

        result =
            saneInt
    in
    result


reasonToReject : Int -> Int -> Maybe String
reasonToReject int currentYear =
    if int <= 0 then
        Just "Anul nașterii nu poate fi negativ."

    else if int > currentYear then
        Just "Anul nașterii este în viitor. Călătorii în timp nu se pot înregistra."

    else if int > currentYear - 7 then
        Just "Sunteți prea tînăr."

    else if int < currentYear - 100 then
        Just "Sunteți prea bătrîn."

    else
        Nothing


birthYearToString : BirthYear -> String
birthYearToString (BirthYear int) =
    String.fromInt int



-- TODO: Get it from the current time somehow?
