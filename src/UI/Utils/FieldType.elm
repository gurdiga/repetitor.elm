module UI.Utils.FieldType exposing (InputType(..), inputTypeToString)


type InputType
    = Email
    | Text
    | PhoneNumber
    | Number


inputTypeToString : InputType -> String
inputTypeToString inputType =
    case inputType of
        Email ->
            "email"

        Text ->
            "text"

        PhoneNumber ->
            "tel"

        Number ->
            "number"
