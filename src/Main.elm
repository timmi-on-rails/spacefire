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


canvasSize : ( number, number )
canvasSize =
    ( 500, 750 )


type alias Ship =
    { x : Float
    , vx : Float
    , width : Float
    , height : Float
    }


type alias Model =
    { ship : Ship
    , pressedKeys : List Key
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { ship =
            { x = 0
            , vx = 0
            , width = 50
            , height = 50
            }
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
                    List.sum
                        [ if List.member ArrowLeft pressedKeys then
                            -0.3

                          else
                            0
                        , if List.member ArrowRight pressedKeys then
                            0.3

                          else
                            0
                        ]
              in
              { model
                | pressedKeys = pressedKeys
                , ship =
                    let
                        ship =
                            model.ship
                    in
                    { ship | vx = vx }
              }
            , Cmd.none
            )

        Tick delta ->
            ( { model
                | ship =
                    let
                        ship =
                            model.ship
                    in
                    { ship
                        | x =
                            let
                                ( w, _ ) =
                                    canvasSize
                            in
                            max (-0.5 * w + 0.5 * ship.width) (min (0.5 * w - 0.5 * ship.width) (ship.x + ship.vx * delta))
                    }
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    toHtml canvasSize
        []
        (shapes [ fill Color.black ]
            [ let
                ( w, h ) =
                    canvasSize
              in
              rect ( 0, 0 ) w h
            ]
            :: renderItems model
        )


renderItems : Model -> List Renderable
renderItems model =
    [ shapes [ fill Color.red ]
        [ let
            ( w, h ) =
                canvasSize
          in
          rect ( 0.5 * w + model.ship.x - 0.5 * model.ship.width, 0.8 * h ) model.ship.width model.ship.height
        ]
    ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
