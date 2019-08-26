module Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName)


type FullName
    = FullName String


makeFullName : String -> Result String FullName
makeFullName string =
    string
        |> String.trim
        |> validateName FullName


validateName : (String -> a) -> String -> Result String a
validateName toValue string =
    if String.length string < 3 then
        Err "Numele pare să fie incorect. (Prea scurt pentru un nume adevărat?)"

    else if String.length string > 50 then
        Err " pare să fie incorect. (Mai mult de 50 de caractere?!)"

    else
        Ok (toValue string)


fullNameToString : FullName -> String
fullNameToString (FullName string) =
    string
