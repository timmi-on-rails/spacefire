module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Color
import Env exposing (Env)
import Game exposing (Game)
import Html
import Html.Attributes
import Mouse


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
init =
    Env.init >> Tuple.mapBoth Initializing (Cmd.map EnvMsg)


type Msg
    = EnvMsg Env.Msg
    | Tick Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnvMsg envMsg ->
            case model of
                Initializing envBuilder ->
                    envBuilder |> Env.updateB envMsg |> Tuple.mapFirst tryInitEnv

                Initialized (Ok env) ->
                    env |> Env.update envMsg |> Tuple.mapFirst tryRunGame

                Initialized (Err _) ->
                    ( model, Cmd.none )

                Running game ->
                    Game.getEnv game
                        |> Env.update envMsg
                        |> Tuple.mapFirst (Game.setEnv game >> Running)

        Tick delta ->
            case model of
                Initializing _ ->
                    ( model, Cmd.none )

                Initialized _ ->
                    ( model, Cmd.none )

                Running game ->
                    ( Running (Game.update delta game), Cmd.none )


tryRunGame : Env -> Model
tryRunGame env =
    if List.length env.pressedButtons > 0 || List.length env.pressedKeys > 0 then
        Game.init env |> Running

    else
        Ok env |> Initialized


tryInitEnv : Env.Builder -> Model
tryInitEnv envBuilder =
    case Env.fromBuilder envBuilder of
        Ok (Just env) ->
            Ok env |> Initialized

        Ok Nothing ->
            envBuilder |> Initializing

        Err err ->
            Initialized (Err err)


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ renderCanvas model
        , Html.div []
            [ Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: Mouse.events (Env.MouseMsg >> EnvMsg) Env.Left
                )
                [ Html.text "←" ]
            , Html.button
                (Html.Attributes.style "width" "50%"
                    :: Html.Attributes.style "height" "50px"
                    :: Mouse.events (Env.MouseMsg >> EnvMsg) Env.Right
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
                , textures = Env.textures EnvMsg
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
        Initializing _ ->
            Sub.none

        Initialized _ ->
            Sub.map EnvMsg Env.subscriptions

        Running _ ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Tick
                , Sub.map EnvMsg Env.subscriptions
                ]
