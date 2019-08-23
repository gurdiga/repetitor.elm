module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.FieldValue exposing (FieldValue(..))
import Html exposing (Attribute, Html, button, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (for, id, required, style, type_, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (RegistrationForm, emptyForm, getFieldValue, updateEmail, updateName, updatePhoneNumber)
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
    , shouldDisplayFullNameErrorMessage : Bool
    , shouldDisplayPhoneNumberErrorMessage : Bool
    , shouldDisplayEmailErrorMessage : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = emptyForm
      , shouldDisplayFullNameErrorMessage = False
      , shouldDisplayPhoneNumberErrorMessage = False
      , shouldDisplayEmailErrorMessage = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateName String
    | ToggleFullNameErrorMessage Bool
    | UpdatePhoneNumber String
    | DisplayPhoneNumberErrorMessage Bool
    | UpdateEmail String
    | DisplayEmailErrorMessage Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        UpdateName string ->
            ( { model | form = updateName form string }, Cmd.none )

        ToggleFullNameErrorMessage bool ->
            ( { model | shouldDisplayFullNameErrorMessage = bool }, Cmd.none )

        UpdatePhoneNumber string ->
            ( { model | form = updatePhoneNumber form string }, Cmd.none )

        DisplayPhoneNumberErrorMessage bool ->
            ( { model | shouldDisplayPhoneNumberErrorMessage = bool }, Cmd.none )

        UpdateEmail string ->
            ( { model | form = updateEmail form string }, Cmd.none )

        DisplayEmailErrorMessage bool ->
            ( { model | shouldDisplayEmailErrorMessage = bool }, Cmd.none )



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
        [ field
            { domId = "full-name"
            , label = "Nume"
            , note = ""
            , fieldValue = getFieldValue model.form .fullName
            , toMsg = UpdateName
            , toggleErrorMessage = ToggleFullNameErrorMessage
            , shouldDisplayErrorMessage = model.shouldDisplayFullNameErrorMessage
            , inputType = Text
            }
        , field
            { domId = "phone-number"
            , label = "Telefon"
            , note = "Veți primi parola pe SMS."
            , fieldValue = getFieldValue model.form .phoneNumber
            , toMsg = UpdatePhoneNumber
            , toggleErrorMessage = DisplayPhoneNumberErrorMessage
            , shouldDisplayErrorMessage = model.shouldDisplayPhoneNumberErrorMessage
            , inputType = PhoneNumber
            }
        , field
            { domId = "email"
            , label = "Email"
            , note = ""
            , fieldValue = getFieldValue model.form .email
            , toMsg = UpdateEmail
            , toggleErrorMessage = DisplayEmailErrorMessage
            , shouldDisplayErrorMessage = model.shouldDisplayEmailErrorMessage
            , inputType = Email
            }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


field : { domId : String, label : String, note : String, fieldValue : FieldValue a, inputType : InputType, toMsg : String -> Msg, toggleErrorMessage : Bool -> Msg, shouldDisplayErrorMessage : Bool } -> Html Msg
field { domId, label, note, fieldValue, inputType, toMsg, toggleErrorMessage, shouldDisplayErrorMessage } =
    let
        ( inputValue, validationMessage, validationMessageColor ) =
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
            , style "background-color"
                (if shouldDisplayErrorMessage then
                    "yellow"

                 else
                    "transparent"
                )
            ]

        inputAttrs =
            [ id domId
            , value inputValue
            , onInput toMsg
            , onFocus (toggleErrorMessage True)
            , onBlur (toggleErrorMessage False)
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
        , p noteStyles [ text note ] |> ifNotEmpty note
        , p validationMessageStyles [ text validationMessage ] |> ifNotEmpty validationMessage
        ]


ifNotEmpty : String -> Html Msg -> Html Msg
ifNotEmpty string content =
    if string == "" then
        text ""

    else
        content


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
