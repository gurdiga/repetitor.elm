module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Tutor.Pages.Registration.Widget as Widget


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
    , body = [ h1 [] [ text "Înregistrare repetitor!", Widget.view ] ]
    }
