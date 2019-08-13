module Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)

import Domain.Utils.Name exposing (validateName)


type FirstName
    = FirstName String


makeFirstName : String -> Result String FirstName
makeFirstName string =
    string
        |> String.trim
        |> validateName "Prenumele" FirstName


firstNameToString : FirstName -> String
firstNameToString (FirstName string) =
    string
