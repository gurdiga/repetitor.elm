module Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)

import Domain.Utils.String exposing (isAllLetters)


type LastName
    = LastName String


makeLastName : String -> Result String LastName
makeLastName string =
    let
        trimmedString =
            String.trim string
    in
    if String.length trimmedString < 2 then
        Err "Numele de familie pare să fie incorect. (O singură litera?!)"

    else if String.length trimmedString > 30 then
        Err "Numele de familie pare să fie incorect. (Mai mult de 30 de litere?!)"

    else if not (isAllLetters trimmedString) then
        Err "Numele de familie pare să fie incorect. (Are simboluri care nu sunt litere?!)"

    else
        Ok (LastName trimmedString)


lastNameToString : LastName -> String
lastNameToString (LastName string) =
    string
