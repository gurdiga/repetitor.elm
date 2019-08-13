module Domain.Utils.Name exposing (validateName)


validateName : String -> (String -> a) -> String -> Result String a
validateName label toValue string =
    if String.length string < 2 then
        Err (label ++ " pare să fie incorect. (O singură litera?!)")

    else if String.length string > 30 then
        Err (label ++ " pare să fie incorect. (Mai mult de 30 de litere?!)")

    else
        Ok (toValue string)
