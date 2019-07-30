module Tutor.Pages.Registration exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



-- path name: inregistrare


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Cmd Msg )
init _ =
    ( {}, Cmd.none )


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


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
        attrs =
            [ style "margin" "0 auto"
            , style "max-width" "400px"
            ]
                ++ additionalAttrs
    in
    div attrs children


registrationForm : Html Msg
registrationForm =
    Html.form []
        [ textField { label = "Prenume", placeHolder = "George" }
        , textField { label = "Nume de familie", placeHolder = "Teodorescu" }
        , textField { label = "Anul nașterii", placeHolder = "1977" }
        , textField { label = "Număr de telefon", placeHolder = "123456789" }
        , textField { label = "Email", placeHolder = "george@gmail.com" }
        ]


textField : { label : String, placeHolder : String } -> Html Msg
textField { label, placeHolder } =
    let
        labelAttrs =
            [ style "display" "grid"
            , style "grid-template-columns" "auto auto"
            ]

        labelTextAttrs =
            [ style "margin-right" "0.5em"
            , style "text-align" "right"
            ]
    in
    layoutRow
        [ Html.label labelAttrs
            [ span labelTextAttrs [ text label ]
            , input [ placeholder placeHolder ] []
            ]
        ]


layoutRow : List (Html Msg) -> Html Msg
layoutRow children =
    let
        attrs =
            [ style "margin-top" "0.5em" ]
    in
    div attrs children
