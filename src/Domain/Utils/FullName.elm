module Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName, maxLength, minLength)


type FullName
    = FullName String


makeFullName : String -> Result String FullName
makeFullName string =
    string
        |> String.trim
        |> validateFullName FullName


validateFullName : (String -> a) -> String -> Result String a
validateFullName toValue string =
    if String.length string < minLength then
        Err "Numele pare să fie incorect. (Prea scurt pentru un nume adevărat?)"

    else if String.length string > maxLength then
        Err "Numele pare să fie incorect. (Mai mult de 50 de caractere?!)"

    else
        Ok (toValue string)


fullNameToString : FullName -> String
fullNameToString (FullName string) =
    string


minLength : Int
minLength =
    3


maxLength : Int
maxLength =
    50
