module Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)


type LastName
    = LastName String


makeLastName : String -> Result String LastName
makeLastName string =
    if String.length string > 0 then
        Ok (LastName string)

    else
        Err "Last name has to have 1 ore more characters"


lastNameToString : LastName -> String
lastNameToString (LastName string) =
    string
