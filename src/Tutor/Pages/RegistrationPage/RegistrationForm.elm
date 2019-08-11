module Tutor.Pages.RegistrationPage.RegistrationForm exposing (FieldValueSet, RegistrationForm, ValueSet, emptyForm, getFieldValue, updateBirthYear, updateEmail, updateFirstName, updateForm, updateLastName, updatePhoneNumber)

import Domain.Utils.BirthYear exposing (BirthYear, birthYearToString, makeBirthYear)
import Domain.Utils.Email exposing (Email, emailToString, makeEmail)
import Domain.Utils.FieldValue exposing (FieldValue(..), fieldFieldValueFromString, makeValidFieldValue)
import Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)
import Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)
import Domain.Utils.PhoneNumber exposing (PhoneNumber, makePhoneNumber, phoneNumberToString)


type RegistrationForm
    = IncompleteRegistrationForm FieldValueSet
    | CompleteRegistrationForm ValueSet


type alias FieldValueSet =
    { firstName : FieldValue FirstName
    , lastName : FieldValue LastName
    , birthYear : FieldValue BirthYear
    , phoneNumber : FieldValue PhoneNumber
    , email : FieldValue Email
    }


type alias ValueSet =
    { firstName : FirstName
    , lastName : LastName
    , birthYear : BirthYear
    , phoneNumber : PhoneNumber
    , email : Email
    }


emptyForm : RegistrationForm
emptyForm =
    IncompleteRegistrationForm
        { firstName = EmptyFieldValue
        , lastName = EmptyFieldValue
        , birthYear = EmptyFieldValue
        , phoneNumber = EmptyFieldValue
        , email = EmptyFieldValue
        }


updateFirstName : RegistrationForm -> String -> RegistrationForm
updateFirstName form string =
    updateForm form (\fieldValues -> { fieldValues | firstName = fieldFieldValueFromString makeFirstName string })


updateLastName : RegistrationForm -> String -> RegistrationForm
updateLastName form string =
    updateForm form (\fieldValues -> { fieldValues | lastName = fieldFieldValueFromString makeLastName string })


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
        ({ firstName, lastName, birthYear, phoneNumber, email } as fieldValues) =
            incompleteFormFields form |> updateFunction
    in
    case ( firstName, lastName, ( birthYear, phoneNumber, email ) ) of
        ( ValidFieldValue _ validFirstName, ValidFieldValue _ validLastName, ( ValidFieldValue _ validBirthYear, ValidFieldValue _ validPhoneNumber, ValidFieldValue _ validEmail ) ) ->
            CompleteRegistrationForm
                { firstName = validFirstName
                , lastName = validLastName
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

        CompleteRegistrationForm { firstName, lastName, birthYear, phoneNumber, email } ->
            { firstName = makeValidFieldValue firstName firstNameToString
            , lastName = makeValidFieldValue lastName lastNameToString
            , birthYear = makeValidFieldValue birthYear birthYearToString
            , phoneNumber = makeValidFieldValue phoneNumber phoneNumberToString
            , email = makeValidFieldValue email emailToString
            }
