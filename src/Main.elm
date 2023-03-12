module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)
import Keyboard exposing (Key(..), RawKey)


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
    = KeyDown RawKey
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown k ->
            case Keyboard.anyKeyOriginal k of
                Just ArrowLeft ->
                    ( { model | vx = -0.1 }, Cmd.none )

                Just ArrowRight ->
                    ( { model | vx = 0.1 }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
        , Keyboard.downs KeyDown
        ]
