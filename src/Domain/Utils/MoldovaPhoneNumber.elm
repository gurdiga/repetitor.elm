module Domain.Utils.MoldovaPhoneNumber exposing (MoldovaPhoneNumber, makeMoldovaPhoneNumber, phoneNumberToString)

import Parser exposing ((|.), (|=), Parser, chompWhile, end, getChompedString, succeed)


type MoldovaPhoneNumber
    = MoldovaPhoneNumber String


makeMoldovaPhoneNumber : String -> Result String MoldovaPhoneNumber
makeMoldovaPhoneNumber string =
    case string |> String.replace " " "" |> Parser.run digitParser of
        Ok parsedDigits ->
            makePhoneNumberFromDigits parsedDigits

        Err errors ->
            case errors of
                error :: [] ->
                    Err ("Număr incorect: simbolul de pe poziția " ++ String.fromInt error.col ++ " nu este o cifră.")

                _ ->
                    Err "Eroare în program la validarea numărului de telefon."


makePhoneNumberFromDigits : String -> Result String MoldovaPhoneNumber
makePhoneNumberFromDigits digits =
    let
        makeErr string =
            Err (string ++ ", de exemplu: 069123456")

        prefix =
            String.slice 0 3 digits
    in
    if String.length digits /= 9 then
        makeErr "Numărul de telefon trebuie să aibă 9 cifre"

    else if not (List.member prefix validPrefixes) then
        makeErr "Numărul de telefon trebuie să înceapă cu 060-069 sau 076-079"

    else
        Ok (MoldovaPhoneNumber digits)


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


digitParser : Parser String
digitParser =
    succeed identity
        |= (chompWhile Char.isDigit |> getChompedString)
        |. end


phoneNumberToString : MoldovaPhoneNumber -> String
phoneNumberToString (MoldovaPhoneNumber string) =
    string
