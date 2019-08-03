module Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)


type FirstName
    = FirstName String


makeFirstName : String -> Result String FirstName
makeFirstName string =
    if String.length string > 1 then
        Ok (FirstName string)

    else
        Err "First name has to have 2 ore more characters"


firstNameToString : FirstName -> String
firstNameToString (FirstName string) =
    string
