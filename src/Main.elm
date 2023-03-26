module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Canvas.Texture
import Color
import Env exposing (Env)
import Game exposing (Game)
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



-- TODO: Threading seeds around is not super fun, so if you really need this, it is best to build your Generator like normal and then just step it all at once at the top of your program.


type Model
    = Initializing Env.Builder
    | Initialized (Result String Env)
    | Running Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( Initializing
        { seed = Nothing
        , fireTexture = Nothing
        , shipTexture = Nothing
        , canvasSize = { width = 500, height = 600 }
        }
    , Random.generate InitSeed Random.independentSeed
    )


type Msg
    = KeyMsg Keyboard.Msg
    | Tick Float
    | InitSeed Random.Seed
    | TextureLoaded String (Env.Builder -> Canvas.Texture.Texture -> Env.Builder) (Maybe Canvas.Texture.Texture)
    | MouseMsg (Mouse.MouseMsg Env.Button)


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
                    ( Running (Game.update delta game), Cmd.none )

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


tryRunGame : Env -> Model
tryRunGame env =
    if List.length env.pressedButtons > 0 || List.length env.pressedKeys > 0 then
        Game.init env |> Running

    else
        Ok env |> Initialized


tryInitEnv : Env.Builder -> Model
tryInitEnv envBuilder =
    case Env.fromBuilder envBuilder of
        Just env ->
            Ok env |> Initialized

        Nothing ->
            envBuilder |> Initializing


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
                    :: Mouse.events MouseMsg Env.Left
                )
                [ Html.text "←" ]
            , Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: Mouse.events MouseMsg Env.Right
                )
                [ Html.text "→" ]
            ]
        ]


renderCanvas : Model -> Html.Html Msg
renderCanvas model =
    case model of
        Initializing envBuilder ->
            Canvas.toHtmlWith
                { width = envBuilder.canvasSize.width
                , height = envBuilder.canvasSize.height
                , textures = textures
                }
                []
                [ Canvas.text [] ( 100, 100 ) "Initializing" ]

        Initialized (Ok _) ->
            Html.text "Initialized"

        Initialized (Err e) ->
            Html.text e

        Running game ->
            let
                { width, height } =
                    Game.getEnv game |> .canvasSize
            in
            Canvas.toHtml
                ( width, height )
                []
                (Canvas.shapes [ Canvas.Settings.fill Color.black ]
                    [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]
                    :: Game.render game
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
