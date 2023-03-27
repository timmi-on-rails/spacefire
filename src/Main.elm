module Main exposing (..)

import Browser
import Browser.Events
import Canvas
import Canvas.Settings
import Color
import Env
import Game exposing (Game)
import Html


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
    | Running Game


type Msg
    = EnvMsg Env.Msg
    | Tick Float


init : () -> ( Model, Cmd Msg )
init =
    Env.init >> Tuple.mapBoth Initializing (Cmd.map EnvMsg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnvMsg envMsg ->
            case model of
                Initializing builder ->
                    builder |> Env.update envMsg |> Tuple.mapFirst tryRunGame

                Running game ->
                    Game.getEnv game
                        |> Env.updateEnv envMsg
                        |> Tuple.mapFirst (Game.setEnv game >> Running)

        Tick delta ->
            case model of
                Initializing _ ->
                    ( model, Cmd.none )

                Running game ->
                    ( Running (Game.update delta game), Cmd.none )


tryRunGame : Env.Builder -> Model
tryRunGame builder =
    case builder of
        Env.Incomplete _ ->
            Initializing builder

        Env.Done env ->
            if
                (env |> Env.pressedButtons |> List.length)
                    > 0
                    || (env |> Env.pressedKeys |> List.length)
                    > 0
            then
                env |> Game.init |> Running

            else
                Initializing builder

        Env.Failed _ _ ->
            Initializing builder


view : Model -> Html.Html Msg
view model =
    case model of
        Initializing (Env.Incomplete partialEnv) ->
            Env.render EnvMsg (Env.Incomplete partialEnv) [ Canvas.text [] ( 100, 100 ) "Initializing" ]

        Initializing (Env.Done _) ->
            Html.text "Initialized"

        Initializing (Env.Failed err _) ->
            Html.text err

        Running game ->
            let
                env =
                    game |> Game.getEnv

                { width, height } =
                    env |> Env.canvasSize
            in
            Env.render EnvMsg
                (Env.Done env)
                (Canvas.shapes [ Canvas.Settings.fill Color.black ]
                    [ Canvas.rect ( 0, 0 ) (toFloat width) (toFloat height) ]
                    :: Game.render game
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initializing _ ->
            Sub.map EnvMsg Env.subscriptions

        Running _ ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Tick
                , Sub.map EnvMsg Env.subscriptions
                ]
