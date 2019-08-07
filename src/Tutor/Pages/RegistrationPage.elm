module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.FieldValue exposing (FieldValue(..))
import Html exposing (Attribute, Html, button, div, h1, input, pre, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onInput)
import Parser exposing (..)
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (RegistrationForm, emptyForm, getFieldValue, updateBirthYear, updateEmail, updateFirstName, updateForm, updateLastName, updatePhoneNumber)



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
    { form : RegistrationForm }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = emptyForm }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateFirstName String
    | UpdateLastName String
    | UpdateBirthYear String
    | UpdatePhoneNumber String
    | UpdateEmail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFirstName string ->
            ( { model | form = updateFirstName model.form string }, Cmd.none )

        UpdateLastName string ->
            ( { model | form = updateLastName model.form string }, Cmd.none )

        UpdateBirthYear string ->
            ( { model | form = updateBirthYear model.form string }, Cmd.none )

        UpdatePhoneNumber string ->
            ( { model | form = updatePhoneNumber model.form string }, Cmd.none )

        UpdateEmail string ->
            ( { model | form = updateEmail model.form string }, Cmd.none )



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
        [ textField { label = "Prenume", placeHolder = "de exemplu George", fieldValue = getFieldValue form .firstName, toMsg = UpdateFirstName }
        , textField { label = "Nume de familie", placeHolder = "de exemplu Teodorescu", fieldValue = getFieldValue form .lastName, toMsg = UpdateLastName }
        , textField { label = "Anul nașterii", placeHolder = "de exemplu 1979", fieldValue = getFieldValue form .birthYear, toMsg = UpdateBirthYear }
        , textField { label = "Număr de telefon", placeHolder = "de exemplu 123456789", fieldValue = getFieldValue form .phoneNumber, toMsg = UpdatePhoneNumber }
        , textField { label = "Email", placeHolder = "de exemplu george@gmail.com", fieldValue = getFieldValue form .email, toMsg = UpdateEmail }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


textField : { label : String, placeHolder : String, fieldValue : FieldValue a, toMsg : String -> Msg } -> Html Msg
textField { label, placeHolder, fieldValue, toMsg } =
    let
        ( inputValue, fieldInfo, fieldInfoColor ) =
            case fieldValue of
                EmptyFieldValue ->
                    ( "", "", "black" )

                InvalidFieldValue textValue errorMessage ->
                    ( textValue, errorMessage, "red" )

                ValidFieldValue textValue _ ->
                    ( textValue, "Bun.", "green" )

        inputStyles =
            [ style "font" "inherit"
            ]

        fieldInfoStyle =
            [ style "grid-column-start" "field"
            , style "color" fieldInfoColor
            ]
    in
    layoutRow
        [ Html.label labelStyles
            [ span labelTextStyles [ text label ]
            , input (inputStyles ++ [ placeholder placeHolder, value inputValue, onInput toMsg ]) []
            , span fieldInfoStyle [ text fieldInfo ]
            ]
        ]


labelStyles : List (Attribute msg)
labelStyles =
    [ style "display" "grid"
    , style "grid-template-columns" "[label] auto [field] 50%"
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


labelTextStyles : List (Attribute msg)
labelTextStyles =
    [ style "margin-right" "0.5em"
    , style "font" "inherit"
    , style "text-align" "right"
    , style "padding" "4px 0"
    ]


layoutRow : List (Html Msg) -> Html Msg
layoutRow children =
    let
        styles =
            [ style "margin-top" "0.5em" ]

        attrs =
            styles
    in
    div attrs children
