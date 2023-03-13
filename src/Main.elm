module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html)
import Keyboard exposing (Key(..))
import Random


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



-- TODO: Threading seeds around is not super fun, so if you really need this, it is best to build your Generator like normal and then just step it all at once at the top of your program.


type alias Model =
    { ship : Ship
    , seed : Random.Seed
    , nextFireEta : Float
    , fires : List ( Float, Float )
    , missedFires : Int
    , pressedKeys : List Key
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ship =
            { x = 0
            , vx = 0
            , width = 50
            , height = 50
            }
      , seed = Random.initialSeed 0
      , nextFireEta = 0
      , fires = []
      , missedFires = 0
      , pressedKeys = []
      }
    , Random.generate InitSeed Random.independentSeed
    )


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float
    | InitSeed Random.Seed


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
            ( model
                |> moveShip delta
                |> decrementFireEta delta
                |> spawnFire
                |> resetFireEta
                |> moveFires delta
                |> handleMissedFires
            , Cmd.none
            )

        InitSeed s ->
            ( { model | seed = s } |> resetFireEta, Cmd.none )


moveFires : Float -> Model -> Model
moveFires delta model =
    let
        move : ( Float, Float ) -> ( Float, Float )
        move f =
            ( Tuple.first f, Tuple.second f + 0.1 * delta )

        newFires =
            model.fires |> List.map move
    in
    { model | fires = newFires }


moveShip : Float -> Model -> Model
moveShip delta model =
    { model
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


resetFireEta : Model -> Model
resetFireEta model =
    if model.nextFireEta <= 0 then
        let
            g =
                Random.float 1000 5000

            ( r, newModel ) =
                step g model
        in
        { newModel | nextFireEta = r }

    else
        model


handleMissedFires : Model -> Model
handleMissedFires model =
    let
        ( _, h ) =
            canvasSize

        cond f =
            Tuple.second f < h

        newFires =
            model.fires |> List.filter cond
    in
    { model | fires = newFires, missedFires = List.length model.fires - List.length newFires }


decrementFireEta : Float -> Model -> Model
decrementFireEta delta model =
    { model | nextFireEta = model.nextFireEta - delta }


spawnFire : Model -> Model
spawnFire model =
    if model.nextFireEta <= 0 then
        let
            ( w, _ ) =
                canvasSize

            g =
                Random.float (-0.5 * w) (0.5 * w)

            ( r, newModel ) =
                step g model
        in
        { newModel | fires = ( r, 0 ) :: model.fires }

    else
        model


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
    , shapes [ fill Color.orange ]
        (fireShapes model)
    ]


fireShapes : Model -> List Shape
fireShapes model =
    let
        ( w, _ ) =
            canvasSize
    in
    model.fires
        |> List.map
            (\f ->
                rect ( 0.5 * w + Tuple.first f - 0.5 * 10, Tuple.second f ) 10 10
            )


step : Random.Generator a -> Model -> ( a, Model )
step generator model =
    let
        ( a, nextSeed ) =
            model.seed |> Random.step generator
    in
    ( a, { model | seed = nextSeed } )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
