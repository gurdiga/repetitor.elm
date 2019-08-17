module Tutor.Pages.RegistrationPage.RegistrationForm exposing (FieldValueSet, RegistrationForm, ValueSet, emptyForm, getFieldValue, updateBirthYear, updateEmail, updateFirstName, updateForm, updatePhoneNumber)

import Domain.Utils.BirthYear exposing (BirthYear, birthYearToString, makeBirthYear)
import Domain.Utils.Email exposing (Email, emailToString, makeEmail)
import Domain.Utils.FieldValue exposing (FieldValue(..), fieldFieldValueFromString, makeValidFieldValue)
import Domain.Utils.FullName exposing (FullName, fullNameToString, makeFullName)
import Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)


type RegistrationForm
    = IncompleteRegistrationForm FieldValueSet
    | CompleteRegistrationForm ValueSet


type alias FieldValueSet =
    { fullName : FieldValue FullName
    , birthYear : FieldValue BirthYear
    , phoneNumber : FieldValue PhoneNumber
    , email : FieldValue Email
    }


type alias ValueSet =
    { fullName : FullName
    , birthYear : BirthYear
    , phoneNumber : PhoneNumber
    , email : Email
    }


emptyForm : RegistrationForm
emptyForm =
    IncompleteRegistrationForm
        { fullName = EmptyFieldValue
        , birthYear = EmptyFieldValue
        , phoneNumber = EmptyFieldValue
        , email = EmptyFieldValue
        }


updateFirstName : RegistrationForm -> String -> RegistrationForm
updateFirstName form string =
    updateForm form (\fieldValues -> { fieldValues | fullName = fieldFieldValueFromString makeFullName string })


updateBirthYear : RegistrationForm -> String -> Int -> RegistrationForm
updateBirthYear form string currentYear =
    updateForm form (\fieldValues -> { fieldValues | birthYear = fieldFieldValueFromString (makeBirthYear currentYear) string })


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
        ({ fullName, birthYear, phoneNumber, email } as fieldValues) =
            incompleteFormFields form |> updateFunction
    in
    case ( fullName, birthYear, ( phoneNumber, email ) ) of
        ( ValidFieldValue _ validFirstName, ValidFieldValue _ validBirthYear, ( ValidFieldValue _ validPhoneNumber, ValidFieldValue _ validEmail ) ) ->
            CompleteRegistrationForm
                { fullName = validFirstName
                , birthYear = validBirthYear
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

        CompleteRegistrationForm { fullName, birthYear, phoneNumber, email } ->
            { fullName = makeValidFieldValue fullName fullNameToString
            , birthYear = makeValidFieldValue birthYear birthYearToString
            , phoneNumber = makeValidFieldValue phoneNumber phoneNumberToString
            , email = makeValidFieldValue email emailToString
            }
