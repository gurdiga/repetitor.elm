module Tutor.Pages.RegistrationPage exposing (main)

import Browser
import Domain.Utils.FieldValue exposing (FieldValue(..), fieldValueFromString)
import Domain.Utils.FirstName exposing (makeFirstName)
import Domain.Utils.LastName exposing (makeLastName)
import Domain.Utils.YearOfBirth exposing (YearOfBirth, makeYearOfBirth)
import Html exposing (Attribute, Html, button, div, h1, input, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Tutor.Pages.RegistrationPage.RegistrationForm exposing (RegistrationForm, emptyForm, updateFirstName, updateForm, updateLastName)



-- path name: inregistrare


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFirstName string ->
            ( { model | form = updateFirstName model.form string }, Cmd.none )

        UpdateLastName string ->
            ( { model | form = updateLastName model.form string }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
        , registrationForm
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


registrationForm : Html Msg
registrationForm =
    let
        styles =
            [ style "max-width" "400px" ]
    in
    Html.form styles
        [ textField { label = "Prenume", placeHolder = "de exemplu George", fieldValue = fieldValueFromString makeFirstName "Ion" }
        , textField { label = "Nume de familie", placeHolder = "de exemplu Teodorescu", fieldValue = fieldValueFromString makeLastName "Teodorescu" }
        , textField { label = "Anul nașterii", placeHolder = "de " ++ Debug.toString (makeYearOfBirth "1989"), fieldValue = EmptyFieldValue }
        , textField { label = "Număr de telefon", placeHolder = "de exemplu 123456789", fieldValue = EmptyFieldValue }
        , textField { label = "Email", placeHolder = "de exemplu george@gmail.com", fieldValue = EmptyFieldValue }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


textField : { label : String, placeHolder : String, fieldValue : FieldValue a } -> Html Msg
textField { label, placeHolder, fieldValue } =
    let
        inputStyles =
            [ style "font" "inherit"
            ]

        inputAttrs =
            inputStyles ++ [ placeholder placeHolder, value inputValue ]

        ( inputValue, fieldInfo, fieldInfoColor ) =
            case fieldValue of
                EmptyFieldValue ->
                    ( "", "", "black" )

                InvalidFieldValue textValue errorMessage ->
                    ( textValue, errorMessage, "red" )

                ValidFieldValue textValue _ ->
                    ( textValue, "Bun.", "green" )

        fieldInfoStyle =
            [ style "grid-column-start" "field"
            , style "color" fieldInfoColor
            ]
    in
    layoutRow
        [ Html.label labelStyles
            [ span labelTextStyles [ text label ]
            , input inputAttrs []
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


type MoldovaPhoneNumber
    = MoldovaPhoneNumber String


type Email
    = Email String
