module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)
import Keyboard exposing (Key(..))


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { x : Float
    , vx : Float
    , pressedKeys : List Key
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { x = 0
      , vx = 0
      , pressedKeys = []
      }
    , Cmd.none
    )


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg k ->
            ( let
                pressedKeys =
                    Keyboard.update k model.pressedKeys

                vx =
                    (if List.member ArrowLeft pressedKeys then
                        -0.3

                     else
                        0
                    )
                        + (if List.member ArrowRight pressedKeys then
                            0.3

                           else
                            0
                          )
              in
              { model
                | pressedKeys = pressedKeys
                , vx = vx
              }
            , Cmd.none
            )

        Tick delta ->
            ( { model | x = max -250 (min 250 (model.x + model.vx * delta)) }, Cmd.none )


view : Model -> Html Msg
view model =
    toHtml ( 500, 500 )
        []
        (shapes [ fill Color.black ] [ rect ( 0, 0 ) 500 500 ]
            :: renderItems model
        )


renderItems : Model -> List Renderable
renderItems model =
    [ shapes [ fill Color.red ] [ rect ( 250 + model.x - 25, 400 ) 50 50 ] ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
