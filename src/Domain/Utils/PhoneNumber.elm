module Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)


type PhoneNumber
    = PhoneNumber String


makePhoneNumber : String -> Result String PhoneNumber
makePhoneNumber string =
    -- TODO: Do proper validation of content and format.
    Ok (PhoneNumber string)


phoneNumberToString : PhoneNumber -> String
phoneNumberToString (PhoneNumber string) =
    string
