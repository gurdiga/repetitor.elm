module Tutor.Pages.Registration exposing (main)

import Browser
import Domain.Utils.YearOfBirth exposing (YearOfBirth, makeYearOfBirth)
import Html exposing (Attribute, Html, button, div, h1, input, span, text)
import Html.Attributes exposing (placeholder, style, type_, value)



-- path name: inregistrare


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { form : RegistrationForm }


type RegistrationForm
    = EmptyRegistrationForm
    | ValidRegistrationForm
    | InvalidRegistrationForm


init : () -> ( Model, Cmd Msg )
init _ =
    ( { form = EmptyRegistrationForm }
    , Cmd.none
    )


type Msg
    = UpdateFormField String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


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
        [ textField { label = "Prenume", placeHolder = "de exemplu George", fieldValue = InvalidFieldValue "Ion" "Hm…" }
        , textField { label = "Nume de familie", placeHolder = "de exemplu Teodorescu", fieldValue = ValidFieldValue "Teodorescu" "Teodorescu" }
        , textField { label = "Anul nașterii", placeHolder = "de " ++ Debug.toString (makeYearOfBirth "1989"), fieldValue = EmptyValue }
        , textField { label = "Număr de telefon", placeHolder = "de exemplu 123456789", fieldValue = EmptyValue }
        , textField { label = "Email", placeHolder = "de exemplu george@gmail.com", fieldValue = EmptyValue }
        , checkBox { label = "Sunt de acord cu condițiile de utilizare" }
        , submitButton { label = "Înregistrează" }
        ]


type alias ErrorMessage =
    String


type alias TextValue =
    String


type FieldValue a
    = ValidFieldValue TextValue a
    | InvalidFieldValue TextValue ErrorMessage
    | EmptyValue


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
                EmptyValue ->
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
    -- TODO: Maybe rename to labelStyle?
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
