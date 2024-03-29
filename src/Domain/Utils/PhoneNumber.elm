module Domain.Utils.PhoneNumber exposing (PhoneNumber, length, makePhoneNumber, phoneNumberToString)

import Parser exposing ((|.), (|=), Parser, chompWhile, end, getChompedString, succeed)


type PhoneNumber
    = PhoneNumber String


makePhoneNumber : String -> Result String PhoneNumber
makePhoneNumber string =
    case string |> String.replace " " "" |> Parser.run digitStringParser of
        Ok parsedDigits ->
            makePhoneNumberFromDigits parsedDigits

        Err errors ->
            case errors of
                error :: [] ->
                    Err ("Număr incorect: simbolul de pe poziția " ++ String.fromInt error.col ++ " nu este o cifră.")

                _ ->
                    Err "Eroare în program la validarea numărului de telefon."


makePhoneNumberFromDigits : String -> Result String PhoneNumber
makePhoneNumberFromDigits digits =
    let
        makeErr string =
            Err (string ++ ", de exemplu: 069123456")

        prefix =
            String.slice 0 3 digits
    in
    if String.length digits > 2 && not (List.member prefix validPrefixes) then
        makeErr "Numărul de telefon trebuie să înceapă cu 060-069 sau 076-079"

    else if String.length digits /= length then
        makeErr "Numărul de telefon trebuie să aibă 9 cifre"

    else
        Ok (PhoneNumber digits)


validPrefixes : List String
validPrefixes =
    let
        prefixRange start end =
            List.range start end
                |> List.map String.fromInt
                |> List.map (\s -> "0" ++ s)
    in
    -- 060-069, 076-079. See https://www.anrceti.md/node/81, anexa
    prefixRange 60 69 ++ prefixRange 76 79


digitStringParser : Parser String
digitStringParser =
    succeed identity
        |= (chompWhile Char.isDigit |> getChompedString)
        |. end


phoneNumberToString : PhoneNumber -> String
phoneNumberToString (PhoneNumber string) =
    string


length : Int
length =
    9
