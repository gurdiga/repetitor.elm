module Domain.Utils.Email exposing (Email, emailToString, makeEmail, maxLength, minLength)

import Regex


type Email
    = Email String


makeEmail : String -> Result String Email
makeEmail string =
    let
        trimmedString =
            String.trim string
    in
    if isValidEmail trimmedString then
        Ok (Email trimmedString)

    else
        Err "Adresa de email pare a fi incorectă."


isValidEmail : String -> Bool
isValidEmail string =
    matchesRegexExactly emailRegex string


matchesRegexExactly : Regex.Regex -> String -> Bool
matchesRegexExactly regex string =
    let
        matches =
            Regex.find regex string
    in
    case matches of
        { match } :: [] ->
            match == string

        _ ->
            False


emailRegex : Regex.Regex
emailRegex =
    -- Thanks to https://emailregex.com/
    Maybe.withDefault Regex.never <|
        Regex.fromString "[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]{2,})+"


emailToString : Email -> String
emailToString (Email string) =
    string


minLength : Int
minLength =
    6


maxLength : Int
maxLength =
    50
