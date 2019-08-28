module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.Email as Email
import Domain.Utils.FieldValue exposing (FieldValue(..), isEmptyFieldValue)
import Domain.Utils.FullName as FullName
import Domain.Utils.PhoneNumber as PhoneNumber
import Html exposing (Attribute, Html, button, div, h1, input, p, pre, span, text)
import Html.Attributes exposing (attribute, autofocus, for, id, maxlength, minlength, novalidate, required, style, type_, value)
import Html.Events exposing (onBlur, onCheck, onClick, onInput, preventDefaultOn)
import Json.Decode as Json exposing (Error(..))
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (Field, RegistrationForm, displayValidationMessageForEmail, displayValidationMessageForFullName, displayValidationMessageForPhoneNumber, emptyForm, getField, updateEmail, updateFullName, updatePhoneNumber, updateSignedUserAgreement, validateFields)
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
    = Noop
    | UpdateFullName String
    | UpdatePhoneNumber String
    | UpdateEmail String
    | UpdateSignedUserAgreement Bool
    | DisplayValidationMessageForFullName Bool
    | DisplayValidationMessageForPhoneNumber Bool
    | DisplayValidationMessageForEmail Bool
    | ValidateFields


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        Noop ->
            ( model, Cmd.none )

        UpdateFullName string ->
            ( { model | form = form |> updateFullName string }, Cmd.none )

        UpdatePhoneNumber string ->
            ( { model | form = form |> updatePhoneNumber string }, Cmd.none )

        UpdateEmail string ->
            ( { model | form = form |> updateEmail string }, Cmd.none )

        UpdateSignedUserAgreement bool ->
            ( { model | form = form |> updateSignedUserAgreement bool }, Cmd.none )

        DisplayValidationMessageForFullName bool ->
            ( { model | form = form |> displayValidationMessageForFullName bool }, Cmd.none )

        DisplayValidationMessageForPhoneNumber bool ->
            ( { model | form = form |> displayValidationMessageForPhoneNumber bool }, Cmd.none )

        DisplayValidationMessageForEmail bool ->
            ( { model | form = form |> displayValidationMessageForEmail bool }, Cmd.none )

        ValidateFields ->
            ( { model | form = validateFields form }, Cmd.none )



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

        attrs =
            [ preventDefaultSubmit True Noop, novalidate True ] ++ styles
    in
    Html.form attrs
        [ formField
            { inputType = Text
            , label = "Nume"
            , field = getField model.form .fullName
            , domId = "full-name"
            , note = "Numele dumneavoastră va fi afișat lîngă cursurile pe care le veți oferi."
            , onInputMsg = UpdateFullName
            , onBlurMsg = DisplayValidationMessageForFullName
            , maxLength = Just FullName.maxLength
            , minLength = Just FullName.minLength
            , autoFocus = True
            }
        , formField
            { inputType = Email
            , label = "Email"
            , field = getField model.form .email
            , domId = "email"
            , note = "Va fi folosit pentru comunicarea operativă."
            , onInputMsg = UpdateEmail
            , onBlurMsg = DisplayValidationMessageForEmail
            , maxLength = Just Email.maxLength
            , minLength = Just Email.minLength
            , autoFocus = False
            }
        , formField
            { inputType = PhoneNumber
            , label = "Telefon"
            , field = getField model.form .phoneNumber
            , domId = "phone-number"
            , note = "Veți primi parola pe SMS."
            , onInputMsg = UpdatePhoneNumber
            , onBlurMsg = DisplayValidationMessageForPhoneNumber
            , maxLength = Just PhoneNumber.length
            , minLength = Just PhoneNumber.length
            , autoFocus = False
            }
        , checkBox
            { label = "Accept Termenii de Utilizare"
            , note = ""
            , domId = "user-agreement"
            , field = getField model.form .signedUserAgreement
            , onCheckMsg = UpdateSignedUserAgreement
            }
        , submitButton { label = "Înregistrează", onClickMsg = ValidateFields }
        ]


preventDefaultSubmit : Bool -> Msg -> Attribute Msg
preventDefaultSubmit bool msg =
    preventDefaultOn "submit" (Json.map (\m -> ( m, bool )) (Json.succeed msg))


formField : { domId : String, label : String, note : String, field : Field a, inputType : InputType, onInputMsg : String -> Msg, onBlurMsg : Bool -> Msg, minLength : Maybe Int, maxLength : Maybe Int, autoFocus : Bool } -> Html Msg
formField { domId, label, note, field, inputType, onInputMsg, onBlurMsg, minLength, maxLength, autoFocus } =
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

        labelAttrs =
            [ for domId ] ++ labelStyles

        inputStyles =
            [ style "font" "inherit" ]

        inputAttrs =
            [ id domId
            , value inputValue
            , onInput onInputMsg
            , onBlur (onBlurMsg (not (isEmptyFieldValue field.value)))
            , required True
            , type_ (inputTypeToString inputType)
            , autofocus autoFocus
            , attribute "aria-described-by" noteDomId
            ]
                ++ attrsFromMaybe maxlength maxLength
                ++ attrsFromMaybe minlength minLength
                ++ inputStyles

        noteStyles =
            [ style "font-size" "0.75em"
            , style "opacity" "0.5"
            , style "grid-column-start" "field"
            , style "margin" "0.2em 0 0"
            ]

        noteDomId =
            domId ++ "-note"

        noteAttrs =
            [ id noteDomId ] ++ noteStyles

        validationMessageStyles =
            [ style "grid-column-start" "field"
            , style "color" validationMessageColor
            ]
    in
    layoutRow
        [ Html.label labelAttrs [ text label ]
        , input inputAttrs []
        , ifNotEmpty note (p noteAttrs [ text note ])
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


attrsFromMaybe : (a -> Attribute Msg) -> Maybe a -> List (Attribute Msg)
attrsFromMaybe attr maybe =
    case maybe of
        Just v ->
            [ attr v ]

        Nothing ->
            []


submitButton : { label : String, onClickMsg : Msg } -> Html Msg
submitButton { label, onClickMsg } =
    let
        styles =
            [ style "font" "inherit" ]

        attrs =
            [ type_ "submit"
            , onClick onClickMsg
            ]
                ++ styles
    in
    layoutRow
        [ button attrs [ text label ]
        ]


checkBox : { label : String, note : String, domId : String, field : Field a, onCheckMsg : Bool -> Msg } -> Html Msg
checkBox { label, note, domId, field, onCheckMsg } =
    let
        ( inputValue, validationMessage, validationMessageColor ) =
            case field.value of
                EmptyFieldValue ->
                    ( "", "", "black" )

                InvalidFieldValue textValue errorMessage ->
                    ( textValue, errorMessage, "red" )

                ValidFieldValue textValue _ ->
                    ( textValue, "Bun.", "green" )

        inputStyles =
            []

        inputAttrs =
            [ type_ "checkbox"
            , value inputValue
            , id domId
            , onCheck onCheckMsg
            ]
                ++ inputStyles

        labelStyles =
            []

        labelAttrs =
            [ for domId ] ++ labelStyles

        noteStyles =
            [ style "font-size" "0.75em"
            , style "opacity" "0.5"
            , style "grid-column-start" "field"
            , style "margin" "0.2em 0 0"
            ]

        validationMessageStyles =
            [ style "color" validationMessageColor
            ]

        divStyles =
            [ style "grid-column-start" "field"
            ]
    in
    layoutRow
        [ div divStyles
            [ input inputAttrs []
            , Html.label labelAttrs [ text label ]
            , ifNotEmpty note (p noteStyles [ text note ])
            , ifTrue
                (field.displayValidationMessage && validationMessage /= "")
                (p validationMessageStyles [ text validationMessage ])
            ]
        ]


layoutRow : List (Html Msg) -> Html Msg
layoutRow children =
    let
        styles =
            [ style "margin-top" "0.5em"
            , style "display" "grid"
            , style "grid-template-columns" "[label] auto [field] 70%"
            ]

        attrs =
            styles
    in
    div attrs children
