module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Texture
import Color
import Html
import Html.Attributes
import Html.Events
import Json.Decode
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
    ( 500, 600 )


fireSize : ( number, number )
fireSize =
    ( 25, 25 )


type alias Ship =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }



-- TODO: Threading seeds around is not super fun, so if you really need this, it is best to build your Generator like normal and then just step it all at once at the top of your program.


type Model
    = Initializing EnvBuilder
    | Initialized (Result String Env)
    | Running Game


type alias EnvBuilder =
    { seed : Maybe Random.Seed
    , shipTexture : Maybe Canvas.Texture.Texture
    , fireTexture : Maybe Canvas.Texture.Texture
    }


type alias Env =
    { seed : Random.Seed
    , pressedKeys : List Keyboard.Key
    , pressedButtons : List Button
    , shipTexture : Canvas.Texture.Texture
    , fireTexture : Canvas.Texture.Texture
    }


type alias Game =
    { env : Env
    , ship : Ship
    , nextFireEta : Float
    , fires : List ( Float, Float )
    , missedFires : Int
    , killedFires : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initializing
        { seed = Nothing
        , fireTexture = Nothing
        , shipTexture = Nothing
        }
    , Random.generate InitSeed Random.independentSeed
    )


newGame : Env -> Game
newGame env =
    { env = env
    , ship =
        { x = 0
        , y = 0.8 * Tuple.second canvasSize
        , width = 50
        , height = 50
        }
    , nextFireEta = 0
    , fires = []
    , missedFires = 0
    , killedFires = 0
    }


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float
    | InitSeed Random.Seed
    | TextureLoaded String (EnvBuilder -> Canvas.Texture.Texture -> EnvBuilder) (Maybe Canvas.Texture.Texture)
    | MouseMsg (MouseMsg Button)


type Button
    = Left
    | Right


type MouseMsg a
    = Up a
    | Down a
    | Leave a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg k ->
            case model of
                Initialized (Ok env) ->
                    ( tryRunGame { env | pressedKeys = Keyboard.update k env.pressedKeys }, Cmd.none )

                Running game ->
                    ( Running
                        { game
                            | env =
                                let
                                    env =
                                        game.env
                                in
                                { env | pressedKeys = Keyboard.update k env.pressedKeys }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Tick delta ->
            case model of
                Running game ->
                    ( Running
                        (game
                            |> moveShip delta
                            |> decrementFireEta delta
                            |> spawnFire
                            |> resetFireEta
                            |> moveFires delta
                            |> handleMissedFires
                            |> handleCollisions
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        InitSeed s ->
            case model of
                Initializing envBuilder ->
                    ( tryInitEnv { envBuilder | seed = Just s }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TextureLoaded name u mt ->
            case mt of
                Just t ->
                    case model of
                        Initializing envBuilder ->
                            ( tryInitEnv (u envBuilder t), Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Nothing ->
                    ( Err ("failed to load texture " ++ name) |> Initialized, Cmd.none )

        MouseMsg x ->
            case model of
                Initialized (Ok env) ->
                    ( tryRunGame { env | pressedButtons = updateButtons x env.pressedButtons }, Cmd.none )

                Running game ->
                    ( Running
                        { game
                            | env =
                                let
                                    env =
                                        game.env
                                in
                                { env | pressedButtons = updateButtons x env.pressedButtons }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


tryRunGame : Env -> Model
tryRunGame env =
    if List.length env.pressedButtons > 0 || List.length env.pressedKeys > 0 then
        newGame env |> resetFireEta |> Running

    else
        Ok env |> Initialized


tryInitEnv : EnvBuilder -> Model
tryInitEnv envBuilder =
    case envBuilder.shipTexture of
        Just shipTexture ->
            case envBuilder.fireTexture of
                Just fireTexture ->
                    case envBuilder.seed of
                        Just seed ->
                            Initialized
                                (Ok
                                    { seed = seed
                                    , fireTexture = fireTexture
                                    , shipTexture = shipTexture
                                    , pressedKeys = []
                                    , pressedButtons = []
                                    }
                                )

                        Nothing ->
                            Initializing envBuilder

                Nothing ->
                    Initializing envBuilder

        Nothing ->
            Initializing envBuilder


updateButtons : MouseMsg a -> List a -> List a
updateButtons mouseMsg buttons =
    case mouseMsg of
        Up x ->
            List.filter ((/=) x) buttons

        Leave x ->
            List.filter ((/=) x) buttons

        Down x ->
            x :: List.filter ((/=) x) buttons


shipVx : Game -> Float
shipVx game =
    List.sum
        [ if
            List.member Keyboard.ArrowLeft game.env.pressedKeys
                || List.member Left game.env.pressedButtons
          then
            -0.3

          else
            0
        , if
            List.member Keyboard.ArrowRight game.env.pressedKeys
                || List.member Right game.env.pressedButtons
          then
            0.3

          else
            0
        ]


moveFires : Float -> Game -> Game
moveFires delta game =
    let
        move : ( Float, Float ) -> ( Float, Float )
        move f =
            ( Tuple.first f, Tuple.second f + 0.1 * delta )

        newFires =
            game.fires |> List.map move
    in
    { game | fires = newFires }


moveShip : Float -> Game -> Game
moveShip delta game =
    { game
        | ship =
            let
                ship =
                    game.ship
            in
            { ship
                | x =
                    let
                        ( w, _ ) =
                            canvasSize
                    in
                    max (-0.5 * w + 0.5 * ship.width) (min (0.5 * w - 0.5 * ship.width) (ship.x + shipVx game * delta))
            }
    }


resetFireEta : Game -> Game
resetFireEta game =
    if game.nextFireEta <= 0 then
        let
            g =
                Random.float 1000 5000

            ( r, newEnv ) =
                step g game.env
        in
        { game | env = newEnv, nextFireEta = r }

    else
        game


handleMissedFires : Game -> Game
handleMissedFires game =
    let
        ( _, h ) =
            canvasSize

        cond f =
            Tuple.second f < h

        newFires =
            game.fires |> List.filter cond
    in
    { game | fires = newFires, missedFires = List.length game.fires - List.length newFires }


handleCollisions : Game -> Game
handleCollisions game =
    let
        ( fw, fh ) =
            fireSize

        noCollision f =
            Tuple.second f
                + 0.5
                * fh
                <= game.ship.y
                || Tuple.second f
                - 0.5
                * fh
                >= game.ship.y
                + game.ship.height
                || Tuple.first f
                + 0.5
                * fw
                <= game.ship.x
                - 0.5
                * game.ship.width
                || Tuple.first f
                - 0.5
                * fw
                >= game.ship.x
                + 0.5
                * game.ship.width

        newFires =
            game.fires |> List.filter noCollision
    in
    { game | fires = newFires, killedFires = List.length game.fires - List.length newFires }


decrementFireEta : Float -> Game -> Game
decrementFireEta delta game =
    { game | nextFireEta = game.nextFireEta - delta }


spawnFire : Game -> Game
spawnFire game =
    if game.nextFireEta <= 0 then
        let
            ( w, _ ) =
                canvasSize

            g =
                Random.float (-0.5 * w) (0.5 * w)

            ( r, newEnv ) =
                step g game.env
        in
        { game | env = newEnv, fires = ( r, 0 ) :: game.fires }

    else
        game


textures : List (Canvas.Texture.Source Msg)
textures =
    [ Canvas.Texture.loadFromImageUrl "./ship.png" (TextureLoaded "ship" (\envBuilder texture -> { envBuilder | shipTexture = Just texture }))
    , Canvas.Texture.loadFromImageUrl "./fire.png" (TextureLoaded "fire" (\envBuilder texture -> { envBuilder | fireTexture = Just texture }))
    ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ renderCanvas model
        , Html.div []
            [ Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: mouseEvents MouseMsg Left
                )
                [ Html.text "←" ]
            , Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: mouseEvents MouseMsg Right
                )
                [ Html.text "→" ]
            ]
        ]


mouseEvents : (MouseMsg a -> msg) -> a -> List (Html.Attribute msg)
mouseEvents f a =
    [ Html.Events.onMouseDown (f (Down a))
    , Html.Events.onMouseUp (f (Up a))
    , Html.Events.onMouseLeave (f (Leave a))
    , Html.Events.on "touchstart" (Json.Decode.succeed (f (Down a)))
    , Html.Events.on "touchend" (Json.Decode.succeed (f (Up a)))
    , Html.Events.on "touchcancel" (Json.Decode.succeed (f (Up a)))
    ]


renderCanvas : Model -> Html.Html Msg
renderCanvas model =
    case model of
        Initializing _ ->
            Canvas.toHtmlWith
                { width = Tuple.first canvasSize
                , height = Tuple.second canvasSize
                , textures = textures
                }
                []
                [ Canvas.text [] ( 100, 100 ) "Initializing" ]

        Initialized (Ok _) ->
            Html.text "Initialized"

        Initialized (Err e) ->
            Html.text e

        Running game ->
            Canvas.toHtml
                canvasSize
                []
                (Canvas.shapes [ Canvas.Settings.fill Color.black ]
                    [ let
                        ( w, h ) =
                            canvasSize
                      in
                      Canvas.rect ( 0, 0 ) w h
                    ]
                    :: renderItems game
                )


renderItems : Game -> List Canvas.Renderable
renderItems game =
    (let
        ( w, _ ) =
            canvasSize
     in
     Canvas.texture [] ( 0.5 * w + game.ship.x - 0.5 * game.ship.width, game.ship.y ) game.env.shipTexture
    )
        :: fireShapes game


fireShapes : Game -> List Canvas.Renderable
fireShapes game =
    let
        ( w, _ ) =
            canvasSize

        ( fw, _ ) =
            fireSize
    in
    game.fires
        |> List.map (\f -> Canvas.texture [] ( 0.5 * w + Tuple.first f - 0.5 * fw, Tuple.second f ) game.env.fireTexture)


step : Random.Generator a -> Env -> ( a, Env )
step generator env =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, { env | seed = nextSeed } )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized _ ->
            Sub.map KeyMsg Keyboard.subscriptions

        Running _ ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Tick
                , Sub.map KeyMsg Keyboard.subscriptions
                ]

        _ ->
            Sub.none
