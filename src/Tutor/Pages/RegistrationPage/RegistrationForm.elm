module Tutor.Pages.RegistrationPage.RegistrationForm exposing (FieldValues, RegistrationForm, Values, emptyForm, updateFirstName, updateForm, updateLastName)

import Domain.Utils.FieldValue exposing (FieldValue(..), fieldValueFromString, makeValidFieldValue)
import Domain.Utils.FirstName exposing (FirstName, firstNameToString, makeFirstName)
import Domain.Utils.LastName exposing (LastName, lastNameToString, makeLastName)


type RegistrationForm
    = IncompleteRegistrationForm FieldValues
    | CompleteRegistrationForm Values


type alias FieldValues =
    { firstName : FieldValue FirstName
    , lastName : FieldValue LastName
    , birthYear : FieldValue Int -- TODO: Replace Int with BirthYear
    , phoneNumber : FieldValue String -- TODO: Replace String with PhoneNumber
    , email : FieldValue String -- TODO: Replace String with Email
    }


type alias Values =
    { firstName : FirstName
    , lastName : LastName
    , birthYear : Int -- TODO: Replace Int with BirthYear
    , phoneNumber : String -- TODO: Replace String with PhoneNumber
    , email : String -- TODO: Replace String with Email
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


incompleteFormFields : RegistrationForm -> FieldValues
incompleteFormFields form =
    case form of
        IncompleteRegistrationForm fields ->
            fields

        CompleteRegistrationForm { firstName, lastName, birthYear, phoneNumber, email } ->
            { firstName = makeValidFieldValue firstName firstNameToString
            , lastName = makeValidFieldValue lastName lastNameToString
            , birthYear = makeValidFieldValue birthYear String.fromInt
            , phoneNumber = makeValidFieldValue phoneNumber (\s -> s)
            , email = makeValidFieldValue email (\s -> s)
            }


updateForm : RegistrationForm -> (FieldValues -> FieldValues) -> RegistrationForm
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


updateFirstName : RegistrationForm -> String -> RegistrationForm
updateFirstName form string =
    updateForm form (\fieldValues -> { fieldValues | firstName = fieldValueFromString makeFirstName string })


updateLastName : RegistrationForm -> String -> RegistrationForm
updateLastName form string =
    updateForm form (\fieldValues -> { fieldValues | lastName = fieldValueFromString makeLastName string })
