module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Texture
import Color
import Game
import Html
import Html.Attributes
import Keyboard
import Mouse
import Random


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


canvasSize : ( Int, Int )
canvasSize =
    ( 500, 600 )



-- TODO: Threading seeds around is not super fun, so if you really need this, it is best to build your Generator like normal and then just step it all at once at the top of your program.


type Model
    = Initializing EnvBuilder
    | Initialized (Result String Game.Env)
    | Running Game.Game


type alias EnvBuilder =
    { seed : Maybe Random.Seed
    , shipTexture : Maybe Canvas.Texture.Texture
    , fireTexture : Maybe Canvas.Texture.Texture
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


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float
    | InitSeed Random.Seed
    | TextureLoaded String (EnvBuilder -> Canvas.Texture.Texture -> EnvBuilder) (Maybe Canvas.Texture.Texture)
    | MouseMsg (Mouse.MouseMsg Game.Button)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg k ->
            case model of
                Initialized (Ok env) ->
                    ( tryRunGame { env | pressedKeys = Keyboard.update k env.pressedKeys }, Cmd.none )

                Running game ->
                    ( game
                        |> Game.updateEnv (\env -> { env | pressedKeys = Keyboard.update k env.pressedKeys })
                        |> Running
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Tick delta ->
            case model of
                Running game ->
                    ( Running (Game.update canvasSize delta game), Cmd.none )

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
                    ( tryRunGame { env | pressedButtons = Mouse.update x env.pressedButtons }, Cmd.none )

                Running game ->
                    ( game
                        |> Game.updateEnv (\env -> { env | pressedButtons = Mouse.update x env.pressedButtons })
                        |> Running
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


tryRunGame : Game.Env -> Model
tryRunGame env =
    if List.length env.pressedButtons > 0 || List.length env.pressedKeys > 0 then
        Game.init canvasSize env |> Running

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
                    :: Mouse.events MouseMsg Game.Left
                )
                [ Html.text "←" ]
            , Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: Mouse.events MouseMsg Game.Right
                )
                [ Html.text "→" ]
            ]
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
                      Canvas.rect ( 0, 0 ) (toFloat w) (toFloat h)
                    ]
                    :: Game.render canvasSize game
                )


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
