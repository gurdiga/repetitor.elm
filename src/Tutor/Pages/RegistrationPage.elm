module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.FieldValue exposing (FieldValue(..))
import Html exposing (Attribute, Html, button, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (placeholder, required, style, type_, value)
import Html.Events exposing (onInput)
import Task
import Time
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (RegistrationForm, emptyForm, getFieldValue, updateBirthYear, updateEmail, updateFirstName, updateForm, updateLastName, updatePhoneNumber)
import UI.Utils.FieldType exposing (InputType(..), inputTypeToString)



-- path name: inregistrare


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { form : RegistrationForm
    , clock :
        { zone : Time.Zone
        , time : Time.Posix
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = emptyForm
      , clock =
            { zone = Time.utc
            , time = Time.millisToPosix 0
            }
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform AdjustTime Time.now
        ]
    )



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateBirthYear String
    | UpdatePhoneNumber String
    | UpdateEmail String
    | AdjustTime Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form, clock } as model) =
    case msg of
        UpdateFirstName string ->
            ( { model | form = updateFirstName form string }, Cmd.none )

        UpdateLastName string ->
            ( { model | form = updateLastName form string }, Cmd.none )

        UpdateBirthYear string ->
            ( { model | form = updateBirthYear form string (getYearFromClock clock) }, Cmd.none )

        UpdatePhoneNumber string ->
            ( { model | form = updatePhoneNumber form string }, Cmd.none )

        UpdateEmail string ->
            ( { model | form = updateEmail form string }, Cmd.none )

        AdjustTime newTime ->
            ( { model | clock = { clock | time = newTime } }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | clock = { clock | zone = newZone } }
            , Cmd.none
            )


getYearFromClock : { zone : Time.Zone, time : Time.Posix } -> Int
getYearFromClock { zone, time } =
    Time.toYear zone time



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Înregistrare repetitor"
    , body = [ pageContents model ]
    }


pageContents : Model -> Html Msg
pageContents model =
    layoutPageContainer []
        [ h1 [] [ text "Înregistrare repetitor" ]
        , registrationForm model.form
        , pre [ style "white-space" "normal" ] [ text (Debug.toString model) ]
        ]


layoutPageContainer : List (Attribute Msg) -> List (Html Msg) -> Html Msg
layoutPageContainer additionalAttrs children =
    let
        styles =
            [ style "margin" "0 auto"
            , style "max-width" "960px"
            , style "font-size" "20px"
            ]

        attrs =
            styles ++ additionalAttrs
    in
    div attrs children


registrationForm : RegistrationForm -> Html Msg
registrationForm form =
    let
        styles =
            [ style "max-width" "400px" ]
    in
    Html.form styles
        [ textField
            { label = "Prenume"
            , note = "De exemplu Petru sau Maria."
            , fieldValue = getFieldValue form .firstName
            , toMsg = UpdateFirstName
            , inputType = Text
            }
        , textField
            { label = "Nume de familie"
            , note = "De exemplu Teodorescu."
            , fieldValue = getFieldValue form .lastName
            , toMsg = UpdateLastName
            , inputType = Text
            }
        , textField
            { label = "Anul nașterii"
            , note = "de exemplu 1979"
            , fieldValue = getFieldValue form .birthYear
            , toMsg = UpdateBirthYear
            , inputType = Number
            }
        , textField
            { label = "Număr de telefon"
            , note = "de exemplu 123456789"
            , fieldValue = getFieldValue form .phoneNumber
            , toMsg = UpdatePhoneNumber
            , inputType = PhoneNumber
            }
        , textField
            { label = "Email"
            , note = "de exemplu george@gmail.com"
            , fieldValue = getFieldValue form .email
            , toMsg = UpdateEmail
            , inputType = Email
            }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


textField : { label : String, note : String, fieldValue : FieldValue a, toMsg : String -> Msg, inputType : InputType } -> Html Msg
textField { label, note, fieldValue, toMsg, inputType } =
    let
        ( inputValue, fieldInfo, fieldInfoColor ) =
            case fieldValue of
                EmptyFieldValue ->
                    ( "", "", "black" )

                InvalidFieldValue textValue errorMessage ->
                    ( textValue, errorMessage, "red" )

                ValidFieldValue textValue _ ->
                    ( textValue, "Bun.", "green" )

        labelStyles =
            [ style "margin-right" "0.5em"
            , style "font" "inherit"
            , style "text-align" "right"
            , style "padding" "4px 0"
            ]

        inputStyles =
            [ style "font" "inherit"
            ]

        noteStyles =
            [ style "font-size" "0.75em"
            , style "opacity" "0.5"
            , style "grid-column-start" "field"
            , style "margin" "0.2em 0 0"
            ]

        errorMessageStyles =
            [ style "grid-column-start" "field"
            , style "color" fieldInfoColor
            ]
    in
    layoutRow
        [ Html.label labelStyles [ text label ]
        , input (inputStyles ++ [ value inputValue, onInput toMsg, required True, type_ (inputTypeToString inputType) ]) []
        , p noteStyles [ text note ]
        , span errorMessageStyles [ text fieldInfo ]
        ]


submitButton : { label : String } -> Html Msg
submitButton { label } =
    let
        styles =
            [ style "font" "inherit" ]

        attrs =
            styles ++ [ type_ "submit" ]
    in
    layoutRow
        [ button attrs [ text label ]
        ]


checkBox : { label : String } -> Html Msg
checkBox { label } =
    let
        attrs =
            [ type_ "checkbox" ]
    in
    layoutRow
        [ Html.label []
            [ input attrs []
            , span [] [ text label ]
            ]
        ]


layoutRow : List (Html Msg) -> Html Msg
layoutRow children =
    let
        styles =
            [ style "margin-top" "0.5em"
            , style "display" "grid"
            , style "grid-template-columns" "[label] auto [field] 50%"
            ]

        attrs =
            styles
    in
    div attrs children
