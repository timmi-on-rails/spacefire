module Main exposing (..)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    ()


init : Model
init =
    ()


type alias Msg =
    ()


update : Msg -> Model -> Model
update _ model =
    model


view : Model -> Html Msg
view _ =
    toHtml ( 500, 500 )
        []
        [ shapes [ fill Color.red ] [ rect ( 0, 0 ) 500 500 ] ]
