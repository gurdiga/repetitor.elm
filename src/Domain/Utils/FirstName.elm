module Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)


type FirstName
    = FirstName String


makeFirstName : String -> Result String FirstName
makeFirstName string =
    -- TODO: trim and validate (regex or parser)
    if String.length string >= 2 && String.length string <= 20 then
        Ok (FirstName string)

    else
        Err "First name has to have between 2 and 20 characters"


firstNameToString : FirstName -> String
firstNameToString (FirstName string) =
    string
