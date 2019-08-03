module Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)


type LastName
    = LastName String


makeLastName : String -> Result String LastName
makeLastName string =
    -- TODO: trim and validate (regex or parser)
    if String.length string >= 1 && String.length string <= 20 then
        Ok (LastName string)

    else
        Err "Last name has to have between 1 and 20 characters"


lastNameToString : LastName -> String
lastNameToString (LastName string) =
    string
