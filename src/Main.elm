module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Texture
import Color
import Html
import Keyboard
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


fireSize : ( number, number )
fireSize =
    ( 25, 25 )


type alias Ship =
    { x : Float
    , y : Float
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
    , killedFires : Int
    , pressedKeys : List Keyboard.Key
    , shipTexture : Load Canvas.Texture.Texture
    , fireTexture : Load Canvas.Texture.Texture
    }


type Load a
    = Loading
    | Success a
    | Failure


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ship =
            { x = 0
            , y = 0.8 * Tuple.second canvasSize
            , vx = 0
            , width = 50
            , height = 50
            }
      , seed = Random.initialSeed 0
      , nextFireEta = 0
      , fires = []
      , missedFires = 0
      , killedFires = 0
      , pressedKeys = []
      , shipTexture = Loading
      , fireTexture = Loading
      }
    , Random.generate InitSeed Random.independentSeed
    )


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float
    | InitSeed Random.Seed
    | TextureLoaded (Model -> Load Canvas.Texture.Texture -> Model) (Maybe Canvas.Texture.Texture)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg k ->
            ( let
                pressedKeys =
                    Keyboard.update k model.pressedKeys

                vx =
                    List.sum
                        [ if List.member Keyboard.ArrowLeft pressedKeys then
                            -0.3

                          else
                            0
                        , if List.member Keyboard.ArrowRight pressedKeys then
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
                |> handleCollisions
            , Cmd.none
            )

        InitSeed s ->
            ( { model | seed = s } |> resetFireEta, Cmd.none )

        TextureLoaded k mt ->
            ( loadTexture model k mt, Cmd.none )


loadTexture : Model -> (Model -> Load Canvas.Texture.Texture -> Model) -> Maybe Canvas.Texture.Texture -> Model
loadTexture model k mt =
    case mt of
        Just t ->
            k model (Success t)

        Nothing ->
            k model Failure


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


handleCollisions : Model -> Model
handleCollisions model =
    let
        ( fw, fh ) =
            fireSize

        noCollision f =
            Tuple.second f
                + 0.5
                * fh
                <= model.ship.y
                || Tuple.second f
                - 0.5
                * fh
                >= model.ship.y
                + model.ship.height
                || Tuple.first f
                + 0.5
                * fw
                <= model.ship.x
                - 0.5
                * model.ship.width
                || Tuple.first f
                - 0.5
                * fw
                >= model.ship.x
                + 0.5
                * model.ship.width

        newFires =
            model.fires |> List.filter noCollision
    in
    { model | fires = newFires, killedFires = List.length model.fires - List.length newFires }


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


textures : List (Canvas.Texture.Source Msg)
textures =
    [ Canvas.Texture.loadFromImageUrl "./ship.png" (TextureLoaded (\model load -> { model | shipTexture = load }))
    , Canvas.Texture.loadFromImageUrl "./fire.png" (TextureLoaded (\model load -> { model | fireTexture = load }))
    ]


view : Model -> Html.Html Msg
view model =
    Canvas.toHtmlWith
        { width = Tuple.first canvasSize
        , height = Tuple.second canvasSize
        , textures = textures
        }
        []
        (Canvas.shapes [ Canvas.Settings.fill Color.black ]
            [ let
                ( w, h ) =
                    canvasSize
              in
              Canvas.rect ( 0, 0 ) w h
            ]
            :: renderItems model
        )


renderItems : Model -> List Canvas.Renderable
renderItems model =
    (case model.shipTexture of
        Failure ->
            Canvas.shapes [] [ Canvas.rect ( 0, 0 ) 0 0 ]

        Success s ->
            let
                ( w, _ ) =
                    canvasSize
            in
            Canvas.texture [] ( 0.5 * w + model.ship.x - 0.5 * model.ship.width, model.ship.y ) s

        Loading ->
            Canvas.shapes [] [ Canvas.rect ( 0, 0 ) 0 0 ]
    )
        :: fireShapes model


fireShapes : Model -> List Canvas.Renderable
fireShapes model =
    let
        ( w, _ ) =
            canvasSize

        ( fw, _ ) =
            fireSize
    in
    model.fires
        |> List.map
            (\f ->
                case model.fireTexture of
                    Failure ->
                        Canvas.shapes [] [ Canvas.rect ( 0, 0 ) 0 0 ]

                    Success s ->
                        Canvas.texture [] ( 0.5 * w + Tuple.first f - 0.5 * fw, Tuple.second f ) s

                    Loading ->
                        Canvas.shapes [] [ Canvas.rect ( 0, 0 ) 0 0 ]
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
        [ Browser.Events.onAnimationFrameDelta Tick
        , Sub.map KeyMsg Keyboard.subscriptions
        ]
