module Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)

import Domain.Utils.Name exposing (allValidSymbols)


type FirstName
    = FirstName String


makeFirstName : String -> Result String FirstName
makeFirstName string =
    let
        trimmedString =
            String.trim string
    in
    if String.length trimmedString < 2 then
        Err "Prenumele pare să fie incorect. (O singură litera?!)"

    else if String.length trimmedString > 30 then
        Err "Prenumele pare să fie incorect. (Mai mult de 30 de litere?!)"

    else if not (allValidSymbols trimmedString) then
        Err "Prenumele pare să fie incorect. (Are simboluri care nu sunt litere?!)"

    else
        Ok (FirstName trimmedString)


firstNameToString : FirstName -> String
firstNameToString (FirstName string) =
    string
