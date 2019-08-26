module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.FieldValue exposing (FieldValue(..))
import Html exposing (Attribute, Html, button, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (for, id, required, style, type_, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode exposing (Error(..))
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (Field, RegistrationForm, displayValidationMessageForEmail, displayValidationMessageForFullName, displayValidationMessageForPhoneNumber, emptyForm, getField, updateEmail, updateFullName, updatePhoneNumber)
import UI.Utils.InputType exposing (InputType(..), inputTypeToString)



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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = emptyForm
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateFullName String
    | UpdatePhoneNumber String
    | UpdateEmail String
    | DisplayValidationMessageForFullName Bool
    | DisplayValidationMessageForPhoneNumber Bool
    | DisplayValidationMessageForEmail Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        UpdateFullName string ->
            ( { model | form = updateFullName form string }, Cmd.none )

        UpdatePhoneNumber string ->
            ( { model | form = updatePhoneNumber form string }, Cmd.none )

        UpdateEmail string ->
            ( { model | form = updateEmail form string }, Cmd.none )

        DisplayValidationMessageForFullName bool ->
            ( { model | form = displayValidationMessageForFullName form bool }, Cmd.none )

        DisplayValidationMessageForPhoneNumber bool ->
            ( { model | form = displayValidationMessageForPhoneNumber form bool }, Cmd.none )

        DisplayValidationMessageForEmail bool ->
            ( { model | form = displayValidationMessageForEmail form bool }, Cmd.none )



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
        , registrationForm model
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


registrationForm : Model -> Html Msg
registrationForm model =
    let
        styles =
            [ style "max-width" "400px" ]
    in
    Html.form styles
        [ formField
            { inputType = Text
            , label = "Nume"
            , field = getField model.form .fullName
            , domId = "full-name"
            , note = ""
            , onInputMsg = UpdateFullName
            , onBlurMsg = DisplayValidationMessageForFullName
            }
        , formField
            { inputType = PhoneNumber
            , label = "Telefon"
            , field = getField model.form .phoneNumber
            , domId = "phone-number"
            , note = "Veți primi parola pe SMS."
            , onInputMsg = UpdatePhoneNumber
            , onBlurMsg = DisplayValidationMessageForPhoneNumber
            }
        , formField
            { inputType = Email
            , label = "Email"
            , field = getField model.form .email
            , domId = "email"
            , note = ""
            , onInputMsg = UpdateEmail
            , onBlurMsg = DisplayValidationMessageForEmail
            }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


formField : { domId : String, label : String, note : String, field : Field a, inputType : InputType, onInputMsg : String -> Msg, onBlurMsg : Bool -> Msg } -> Html Msg
formField { domId, label, note, field, inputType, onInputMsg, onBlurMsg } =
    let
        ( inputValue, validationMessage, validationMessageColor ) =
            case field.value of
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

        inputAttrs =
            [ id domId
            , value inputValue
            , onInput onInputMsg

            -- , onFocus (toggleErrorMessage True)
            , onBlur (onBlurMsg (inputValue /= ""))
            , required True
            , type_ (inputTypeToString inputType)
            ]
                ++ inputStyles

        noteStyles =
            [ style "font-size" "0.75em"
            , style "opacity" "0.5"
            , style "grid-column-start" "field"
            , style "margin" "0.2em 0 0"
            ]

        validationMessageStyles =
            [ style "grid-column-start" "field"
            , style "color" validationMessageColor
            ]
    in
    layoutRow
        [ Html.label (labelStyles ++ [ for domId ]) [ text label ]
        , input inputAttrs []
        , ifNotEmpty note (p noteStyles [ text note ])
        , ifTrue
            (field.displayValidationMessage && validationMessage /= "")
            (p validationMessageStyles [ text validationMessage ])
        ]


ifNotEmpty : String -> Html Msg -> Html Msg
ifNotEmpty string content =
    if string == "" then
        text ""

    else
        content


ifTrue : Bool -> Html Msg -> Html Msg
ifTrue bool content =
    if bool then
        content

    else
        text ""


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
