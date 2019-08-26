module Domain.Utils.FieldValue exposing (FieldValue(..), fieldFieldValueFromString, makeValidFieldValue)


type alias ErrorMessage =
    String


type FieldValue a
    = ValidFieldValue String a
    | InvalidFieldValue String ErrorMessage
    | EmptyFieldValue


fieldFieldValueFromString : (String -> Result String a) -> String -> FieldValue a
fieldFieldValueFromString makeFunction string =
    case makeFunction string of
        Ok value ->
            ValidFieldValue string value

        Err errorMessage ->
            InvalidFieldValue string errorMessage


makeValidFieldValue : a -> (a -> String) -> FieldValue a
makeValidFieldValue value toString =
    ValidFieldValue (toString value) value
