module Domain.Utils.Email exposing (Email, emailToString, makeEmail)


type Email
    = Email String


makeEmail : String -> Result String Email
makeEmail string =
    -- TODO: Do proper validation of content and format.
    Ok (Email string)


emailToString : Email -> String
emailToString (Email string) =
    string
