module Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)

import Domain.Utils.Name exposing (validateName)


type LastName
    = LastName String


makeLastName : String -> Result String LastName
makeLastName string =
    string
        |> String.trim
        |> validateName "Numele de familie" LastName


lastNameToString : LastName -> String
lastNameToString (LastName string) =
    string
