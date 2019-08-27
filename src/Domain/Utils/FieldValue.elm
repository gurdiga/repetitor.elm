module Domain.Utils.FieldValue exposing (FieldValue(..), isEmptyFieldValue, makeFieldValueFromBool, makeFieldValueFromString, makeValidFieldValue)


type alias ErrorMessage =
    String


type FieldValue a
    = ValidFieldValue String a
    | InvalidFieldValue String ErrorMessage
    | EmptyFieldValue


makeFieldValueFromString : (String -> Result String a) -> String -> FieldValue a
makeFieldValueFromString makeFunction string =
    case makeFunction string of
        Ok value ->
            ValidFieldValue string value

        Err errorMessage ->
            InvalidFieldValue string errorMessage


makeFieldValueFromBool : (Bool -> Result String a) -> Bool -> FieldValue a
makeFieldValueFromBool makeFunction bool =
    case makeFunction bool of
        Ok value ->
            ValidFieldValue "True" value

        Err errorMessage ->
            InvalidFieldValue "False" errorMessage


makeValidFieldValue : a -> (a -> String) -> FieldValue a
makeValidFieldValue value toString =
    ValidFieldValue (toString value) value


isEmptyFieldValue : FieldValue a -> Bool
isEmptyFieldValue fieldValue =
    case fieldValue of
        EmptyFieldValue ->
            True

        _ ->
            False
