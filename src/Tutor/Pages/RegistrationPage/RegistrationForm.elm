module Tutor.Pages.RegistrationPage.RegistrationForm exposing (FieldValueSet, RegistrationForm, ValueSet, emptyForm, getFieldValue, updateEmail, updateForm, updateFullName, updatePhoneNumber)

import Domain.Utils.Email exposing (Email, emailToString, makeEmail)
import Domain.Utils.FieldValue exposing (FieldValue(..), fieldFieldValueFromString, makeValidFieldValue)
import Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName)
import Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)


type RegistrationForm
    = IncompleteRegistrationForm FieldValueSet
    | CompleteRegistrationForm ValueSet


type alias FieldValueSet =
    { fullName : FieldValue FullName
    , phoneNumber : FieldValue PhoneNumber
    , email : FieldValue Email
    }


type alias ValueSet =
    { fullName : FullName
    , phoneNumber : PhoneNumber
    , email : Email
    }


emptyForm : RegistrationForm
emptyForm =
    IncompleteRegistrationForm
        { fullName = EmptyFieldValue
        , phoneNumber = EmptyFieldValue
        , email = EmptyFieldValue
        }


updateFullName : RegistrationForm -> String -> RegistrationForm
updateFullName form string =
    updateForm form (\fieldValues -> { fieldValues | fullName = fieldFieldValueFromString makeFullName string })


updatePhoneNumber : RegistrationForm -> String -> RegistrationForm
updatePhoneNumber form string =
    updateForm form (\fieldValues -> { fieldValues | phoneNumber = fieldFieldValueFromString makePhoneNumber string })


updateEmail : RegistrationForm -> String -> RegistrationForm
updateEmail form string =
    updateForm form (\fieldValues -> { fieldValues | email = fieldFieldValueFromString makeEmail string })


getFieldValue : RegistrationForm -> (FieldValueSet -> FieldValue a) -> FieldValue a
getFieldValue form accessFunction =
    incompleteFormFields form |> accessFunction


updateForm : RegistrationForm -> (FieldValueSet -> FieldValueSet) -> RegistrationForm
updateForm form updateFunction =
    let
        ({ fullName, phoneNumber, email } as fieldValues) =
            incompleteFormFields form |> updateFunction
    in
    case ( fullName, phoneNumber, email ) of
        ( ValidFieldValue _ validFirstName, ValidFieldValue _ validPhoneNumber, ValidFieldValue _ validEmail ) ->
            CompleteRegistrationForm
                { fullName = validFirstName
                , phoneNumber = validPhoneNumber
                , email = validEmail
                }

        _ ->
            IncompleteRegistrationForm fieldValues


incompleteFormFields : RegistrationForm -> FieldValueSet
incompleteFormFields form =
    case form of
        IncompleteRegistrationForm fields ->
            fields

        CompleteRegistrationForm { fullName, phoneNumber, email } ->
            { fullName = makeValidFieldValue fullName fullNameToString
            , phoneNumber = makeValidFieldValue phoneNumber phoneNumberToString
            , email = makeValidFieldValue email emailToString
            }
