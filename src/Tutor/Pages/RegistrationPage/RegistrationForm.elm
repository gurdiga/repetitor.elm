module Tutor.Pages.RegistrationPage.RegistrationForm exposing (FieldSet, RegistrationForm, ValueSet, emptyForm, getFieldValue, updateEmail, updateForm, updateName, updatePhoneNumber)

import Domain.Utils.Email exposing (Email, emailToString, makeEmail)
import Domain.Utils.FieldValue exposing (FieldValue(..), fieldFieldValueFromString, makeValidFieldValue)
import Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName)
import Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)


type RegistrationForm
    = IncompleteRegistrationForm FieldSet
    | CompleteRegistrationForm ValueSet


type alias Field a =
    { value : FieldValue a
    , hasBeenBlurred : Bool
    }


emptyField : Field a
emptyField =
    { value = EmptyFieldValue
    , hasBeenBlurred = False
    }


type alias FieldSet =
    { fullName : Field FullName
    , phoneNumber : Field PhoneNumber
    , email : Field Email
    }


type alias ValueSet =
    { fullName : FullName
    , phoneNumber : PhoneNumber
    , email : Email
    }


emptyForm : RegistrationForm
emptyForm =
    IncompleteRegistrationForm
        { fullName = emptyField
        , phoneNumber = emptyField
        , email = emptyField
        }


updateName : RegistrationForm -> String -> RegistrationForm
updateName form string =
    updateForm form
        (\({ fullName } as fields) ->
            { fields | fullName = { fullName | value = fieldFieldValueFromString makeFullName string } }
        )


updatePhoneNumber : RegistrationForm -> String -> RegistrationForm
updatePhoneNumber form string =
    updateForm form
        (\({ phoneNumber } as fields) ->
            { fields | phoneNumber = { phoneNumber | value = fieldFieldValueFromString makePhoneNumber string } }
        )


updateEmail : RegistrationForm -> String -> RegistrationForm
updateEmail form string =
    updateForm form
        (\({ email } as fields) ->
            { fields | email = { email | value = fieldFieldValueFromString makeEmail string } }
        )


getFieldValue : RegistrationForm -> (FieldSet -> Field a) -> FieldValue a
getFieldValue form accessFunction =
    incompleteFormFields form |> accessFunction |> .value


updateForm : RegistrationForm -> (FieldSet -> FieldSet) -> RegistrationForm
updateForm form updateFunction =
    let
        ({ fullName, phoneNumber, email } as fields) =
            incompleteFormFields form |> updateFunction
    in
    case ( fullName.value, phoneNumber.value, email.value ) of
        ( ValidFieldValue _ validFirstName, ValidFieldValue _ validPhoneNumber, ValidFieldValue _ validEmail ) ->
            CompleteRegistrationForm
                { fullName = validFirstName
                , phoneNumber = validPhoneNumber
                , email = validEmail
                }

        _ ->
            IncompleteRegistrationForm fields


incompleteFormFields : RegistrationForm -> FieldSet
incompleteFormFields form =
    case form of
        IncompleteRegistrationForm fields ->
            fields

        CompleteRegistrationForm { fullName, phoneNumber, email } ->
            { fullName = { value = makeValidFieldValue fullName fullNameToString, hasBeenBlurred = True }
            , phoneNumber = { value = makeValidFieldValue phoneNumber phoneNumberToString, hasBeenBlurred = True }
            , email = { value = makeValidFieldValue email emailToString, hasBeenBlurred = True }
            }
