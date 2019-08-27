module Tutor.Pages.RegistrationPage.RegistrationForm exposing (Field, FieldSet, RegistrationForm, ValueSet, displayValidationMessageForEmail, displayValidationMessageForFullName, displayValidationMessageForPhoneNumber, emptyForm, getField, updateEmail, updateForm, updateFullName, updatePhoneNumber, updateSignedUserAgreement, validateFields)

import Domain.Utils.Email exposing (Email, emailToString, makeEmail)
import Domain.Utils.FieldValue exposing (FieldValue(..), isEmptyFieldValue, makeFieldValueFromBool, makeFieldValueFromString, makeValidFieldValue)
import Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName)
import Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)
import Domain.Utils.SignedUserAgreement exposing (SignedUserAgreement, makeSignedUserAgreement, signedUserAgreementToString)


type RegistrationForm
    = IncompleteRegistrationForm FieldSet
    | CompleteRegistrationForm ValueSet


type alias Field a =
    { value : FieldValue a
    , displayValidationMessage : Bool
    }


emptyField : Field a
emptyField =
    { value = EmptyFieldValue
    , displayValidationMessage = False
    }


type alias FieldSet =
    { fullName : Field FullName
    , phoneNumber : Field PhoneNumber
    , email : Field Email
    , signedUserAgreement : Field SignedUserAgreement
    }


type alias ValueSet =
    { fullName : FullName
    , phoneNumber : PhoneNumber
    , email : Email
    , signedUserAgreement : SignedUserAgreement
    }


emptyForm : RegistrationForm
emptyForm =
    IncompleteRegistrationForm
        { fullName = emptyField
        , phoneNumber = emptyField
        , email = emptyField
        , signedUserAgreement = emptyField
        }


updateFullName : String -> RegistrationForm -> RegistrationForm
updateFullName string form =
    updateForm form
        (\({ fullName } as fields) ->
            { fields | fullName = { fullName | value = makeFieldValueFromString makeFullName string } }
        )


updatePhoneNumber : String -> RegistrationForm -> RegistrationForm
updatePhoneNumber string form =
    updateForm form
        (\({ phoneNumber } as fields) ->
            { fields | phoneNumber = { phoneNumber | value = makeFieldValueFromString makePhoneNumber string } }
        )


updateEmail : String -> RegistrationForm -> RegistrationForm
updateEmail string form =
    updateForm form
        (\({ email } as fields) ->
            { fields | email = { email | value = makeFieldValueFromString makeEmail string } }
        )


updateSignedUserAgreement : Bool -> RegistrationForm -> RegistrationForm
updateSignedUserAgreement bool form =
    updateForm form
        (\({ signedUserAgreement } as fields) ->
            { fields | signedUserAgreement = { signedUserAgreement | value = makeFieldValueFromBool makeSignedUserAgreement bool } }
        )


displayValidationMessageForFullName : Bool -> RegistrationForm -> RegistrationForm
displayValidationMessageForFullName bool form =
    updateForm form
        (\({ fullName } as fields) ->
            { fields | fullName = { fullName | displayValidationMessage = bool } }
        )


displayValidationMessageForPhoneNumber : Bool -> RegistrationForm -> RegistrationForm
displayValidationMessageForPhoneNumber bool form =
    updateForm form
        (\({ phoneNumber } as fields) ->
            { fields | phoneNumber = { phoneNumber | displayValidationMessage = bool } }
        )


displayValidationMessageForEmail : Bool -> RegistrationForm -> RegistrationForm
displayValidationMessageForEmail bool form =
    updateForm form
        (\({ email } as fields) ->
            { fields | email = { email | displayValidationMessage = bool } }
        )


displayValidationMessageForSignedUserAgreement : Bool -> RegistrationForm -> RegistrationForm
displayValidationMessageForSignedUserAgreement bool form =
    updateForm form
        (\({ signedUserAgreement } as fields) ->
            { fields | signedUserAgreement = { signedUserAgreement | displayValidationMessage = bool } }
        )


validateFields : RegistrationForm -> RegistrationForm
validateFields form =
    let
        { fullName, email, phoneNumber, signedUserAgreement } =
            incompleteFormFields form

        f0 =
            form

        f1 =
            if isEmptyFieldValue fullName.value then
                f0
                    |> updateFullName ""
                    |> displayValidationMessageForFullName True

            else
                f0

        f2 =
            if isEmptyFieldValue email.value then
                f1
                    |> updateEmail ""
                    |> displayValidationMessageForEmail True

            else
                f1

        f3 =
            if isEmptyFieldValue phoneNumber.value then
                f2
                    |> updatePhoneNumber ""
                    |> displayValidationMessageForPhoneNumber True

            else
                f2

        f4 =
            if isEmptyFieldValue signedUserAgreement.value then
                f3
                    |> updateSignedUserAgreement False
                    |> displayValidationMessageForSignedUserAgreement True

            else
                f3
    in
    f4


getField : RegistrationForm -> (FieldSet -> Field a) -> Field a
getField form accessFunction =
    incompleteFormFields form |> accessFunction


updateForm : RegistrationForm -> (FieldSet -> FieldSet) -> RegistrationForm
updateForm form updateFunction =
    let
        ({ fullName, phoneNumber, email, signedUserAgreement } as fields) =
            incompleteFormFields form |> updateFunction
    in
    case ( fullName.value, phoneNumber.value, ( email.value, signedUserAgreement.value ) ) of
        ( ValidFieldValue _ validFirstName, ValidFieldValue _ validPhoneNumber, ( ValidFieldValue _ validEmail, ValidFieldValue _ validSignedUserAgreement ) ) ->
            CompleteRegistrationForm
                { fullName = validFirstName
                , phoneNumber = validPhoneNumber
                , email = validEmail
                , signedUserAgreement = validSignedUserAgreement
                }

        _ ->
            IncompleteRegistrationForm fields


incompleteFormFields : RegistrationForm -> FieldSet
incompleteFormFields form =
    case form of
        IncompleteRegistrationForm fields ->
            fields

        CompleteRegistrationForm { fullName, phoneNumber, email, signedUserAgreement } ->
            { fullName = { value = makeValidFieldValue fullName fullNameToString, displayValidationMessage = True }
            , phoneNumber = { value = makeValidFieldValue phoneNumber phoneNumberToString, displayValidationMessage = True }
            , email = { value = makeValidFieldValue email emailToString, displayValidationMessage = True }
            , signedUserAgreement = { value = makeValidFieldValue signedUserAgreement signedUserAgreementToString, displayValidationMessage = True }
            }
