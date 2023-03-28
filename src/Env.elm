module Env exposing
    ( Builder(..)
    , Button(..)
    , Env
    , Msg
    , canvasSize
    , init
    , pressedButtons
    , pressedKeys
    , render
    , step
    , subscriptions
    , textures
    , update
    , updateEnv
    )

import Canvas
import Canvas.Texture
import Html
import Html.Attributes
import Keyboard
import Mouse
import Random


type Builder
    = Incomplete PartialEnv
    | Done Env
    | Failed String PartialEnv


type PartialEnv
    = PartialEnv
        { seed : Maybe Random.Seed
        , shipTexture : Load Canvas.Texture.Texture
        , fireTexture : Load Canvas.Texture.Texture
        , canvasSize : { width : Int, height : Int }
        }


bindLoad : (a -> Load b) -> Load a -> Load b
bindLoad f load =
    case load of
        Loading ->
            Loading

        Success s ->
            f s

        Failure err ->
            Failure err


type Load a
    = Loading
    | Success a
    | Failure String


type Env
    = Env
        { seed : Random.Seed
        , pressedKeys : List Keyboard.Key
        , pressedButtons : List Button
        , shipTexture : Canvas.Texture.Texture
        , fireTexture : Canvas.Texture.Texture
        , canvasSize : { width : Int, height : Int }
        }


type Msg
    = KeyMsg Keyboard.Msg
    | InitSeed Random.Seed
    | TextureLoaded String (PartialEnv -> Load Canvas.Texture.Texture -> PartialEnv) (Maybe Canvas.Texture.Texture)
    | MouseMsg (Mouse.MouseMsg Button)


type Button
    = Left
    | Right


init : () -> ( Builder, Cmd Msg )
init _ =
    ( { seed = Nothing
      , fireTexture = Loading
      , shipTexture = Loading
      , canvasSize = { width = 500, height = 600 }
      }
        |> PartialEnv
        |> Incomplete
    , Random.generate InitSeed Random.independentSeed
    )


updatePressedKeys : Keyboard.Msg -> { a | pressedKeys : List Keyboard.Key } -> { a | pressedKeys : List Keyboard.Key }
updatePressedKeys k x =
    { x | pressedKeys = Keyboard.update k x.pressedKeys }


updatePressedButtons : Mouse.MouseMsg a -> { b | pressedButtons : List a } -> { b | pressedButtons : List a }
updatePressedButtons m x =
    { x | pressedButtons = Mouse.update m x.pressedButtons }


update : Msg -> Builder -> ( Builder, Cmd msg )
update msg builder =
    case builder of
        Incomplete partialEnv ->
            partialEnv |> updatePartial msg |> Tuple.mapFirst complete

        Done env ->
            updateEnv msg env |> Tuple.mapFirst Done

        Failed err partialEnv ->
            ( Failed err partialEnv, Cmd.none )


updatePartial : Msg -> PartialEnv -> ( PartialEnv, Cmd msg )
updatePartial msg (PartialEnv partialEnv) =
    case msg of
        KeyMsg _ ->
            ( PartialEnv partialEnv, Cmd.none )

        MouseMsg _ ->
            ( PartialEnv partialEnv, Cmd.none )

        InitSeed s ->
            ( PartialEnv { partialEnv | seed = Just s }, Cmd.none )

        TextureLoaded name u mt ->
            case mt of
                Just t ->
                    ( u (PartialEnv partialEnv) (Success t), Cmd.none )

                Nothing ->
                    ( "failed to load texture " ++ name |> Failure |> u (PartialEnv partialEnv), Cmd.none )


updateEnv : Msg -> Env -> ( Env, Cmd msg )
updateEnv msg (Env env) =
    case msg of
        KeyMsg k ->
            ( updatePressedKeys k env |> Env, Cmd.none )

        MouseMsg m ->
            ( updatePressedButtons m env |> Env, Cmd.none )

        InitSeed _ ->
            ( Env env, Cmd.none )

        TextureLoaded _ _ _ ->
            ( Env env, Cmd.none )


loadTextures : (Msg -> msg) -> List (Canvas.Texture.Source msg)
loadTextures f =
    [ Canvas.Texture.loadFromImageUrl "./ship.png" (f << TextureLoaded "ship" (\(PartialEnv partialEnv) texture -> PartialEnv { partialEnv | shipTexture = texture }))
    , Canvas.Texture.loadFromImageUrl "./fire.png" (f << TextureLoaded "fire" (\(PartialEnv partialEnv) texture -> PartialEnv { partialEnv | fireTexture = texture }))
    ]


canvasSize : Env -> { width : Int, height : Int }
canvasSize (Env env) =
    env.canvasSize


pressedKeys : Env -> List Keyboard.Key
pressedKeys (Env env) =
    env.pressedKeys


pressedButtons : Env -> List Button
pressedButtons (Env env) =
    env.pressedButtons


textures : Env -> { fireTexture : Canvas.Texture.Texture, shipTexture : Canvas.Texture.Texture }
textures (Env env) =
    { fireTexture = env.fireTexture, shipTexture = env.shipTexture }


step : Random.Generator a -> Env -> ( a, Env )
step generator (Env env) =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, Env { env | seed = nextSeed } )


complete : PartialEnv -> Builder
complete (PartialEnv partialEnv) =
    partialEnv.shipTexture
        |> bindLoad
            (\shipTexture ->
                partialEnv.fireTexture
                    |> bindLoad
                        (\fireTexture ->
                            case partialEnv.seed of
                                Just seed ->
                                    { seed = seed
                                    , fireTexture = fireTexture
                                    , shipTexture = shipTexture
                                    , pressedKeys = []
                                    , pressedButtons = []
                                    , canvasSize = partialEnv.canvasSize
                                    }
                                        |> Env
                                        |> Success

                                Nothing ->
                                    Loading
                        )
            )
        |> toBuilder (PartialEnv partialEnv)


toBuilder : PartialEnv -> Load Env -> Builder
toBuilder partialEnv load =
    case load of
        Loading ->
            Incomplete partialEnv

        Success s ->
            Done s

        Failure err ->
            Failed err partialEnv


subscriptions : Sub Msg
subscriptions =
    Sub.map KeyMsg Keyboard.subscriptions


render : (Msg -> msg) -> Builder -> List Canvas.Renderable -> List (Html.Html msg)
render f builder lst =
    [ canvasStyle
    , renderCanvas f builder lst
    , Html.div []
        [ Html.button
            (Html.Attributes.style "width" "50%"
                :: Html.Attributes.style "height" "50px"
                :: Mouse.events MouseMsg Left
            )
            [ Html.text "←" ]
        , Html.button
            (Html.Attributes.style "width" "50%"
                :: Html.Attributes.style "height" "50px"
                :: Mouse.events MouseMsg Right
            )
            [ Html.text "→" ]
        ]
        |> Html.map f
    ]


renderCanvas : (Msg -> msg) -> Builder -> List Canvas.Renderable -> Html.Html msg
renderCanvas f builder lst =
    case builder of
        Done (Env env) ->
            Canvas.toHtml ( env.canvasSize.width, env.canvasSize.height ) [] lst

        Incomplete (PartialEnv partialEnv) ->
            Canvas.toHtmlWith
                { width = partialEnv.canvasSize.width
                , height = partialEnv.canvasSize.height
                , textures = loadTextures f
                }
                []
                lst

        Failed _ (PartialEnv partialEnv) ->
            Canvas.toHtml ( partialEnv.canvasSize.width, partialEnv.canvasSize.height ) [] lst


canvasStyle : Html.Html msg
canvasStyle =
    Html.node "style"
        []
        [ Html.text
            "canvas { border: 5px solid gray; margin: auto; display: block; }"
        ]
