module Env exposing (Builder, Button(..), Env, Load(..), Msg(..), fromBuilder, init, step, subscriptions, textures, update, updateB)

import Canvas.Texture
import Keyboard
import Mouse
import Random


type alias Builder =
    { seed : Maybe Random.Seed
    , pressedKeys : List Keyboard.Key
    , pressedButtons : List Button
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


type alias Env =
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
    | TextureLoaded String (Builder -> Load Canvas.Texture.Texture -> Builder) (Maybe Canvas.Texture.Texture)
    | MouseMsg (Mouse.MouseMsg Button)


type Button
    = Left
    | Right


init : () -> ( Builder, Cmd Msg )
init _ =
    ( { seed = Nothing
      , pressedKeys = []
      , pressedButtons = []
      , fireTexture = Loading
      , shipTexture = Loading
      , canvasSize = { width = 500, height = 600 }
      }
    , Random.generate InitSeed Random.independentSeed
    )


updatePressedKeys : Keyboard.Msg -> { a | pressedKeys : List Keyboard.Key } -> { a | pressedKeys : List Keyboard.Key }
updatePressedKeys k x =
    { x | pressedKeys = Keyboard.update k x.pressedKeys }


updatePressedButtons : Mouse.MouseMsg a -> { b | pressedButtons : List a } -> { b | pressedButtons : List a }
updatePressedButtons m x =
    { x | pressedButtons = Mouse.update m x.pressedButtons }


updateB : Msg -> Builder -> ( Builder, Cmd msg )
updateB msg builder =
    case msg of
        KeyMsg k ->
            ( updatePressedKeys k builder, Cmd.none )

        MouseMsg m ->
            ( updatePressedButtons m builder, Cmd.none )

        InitSeed s ->
            ( { builder | seed = Just s }, Cmd.none )

        TextureLoaded name u mt ->
            case mt of
                Just t ->
                    ( u builder (Success t), Cmd.none )

                Nothing ->
                    ( "failed to load texture " ++ name |> Failure |> u builder, Cmd.none )


update : Msg -> Env -> ( Env, Cmd msg )
update msg env =
    case msg of
        KeyMsg k ->
            ( updatePressedKeys k env, Cmd.none )

        MouseMsg m ->
            ( updatePressedButtons m env, Cmd.none )

        InitSeed _ ->
            ( env, Cmd.none )

        TextureLoaded _ _ _ ->
            ( env, Cmd.none )


textures : (Msg -> msg) -> List (Canvas.Texture.Source msg)
textures f =
    [ Canvas.Texture.loadFromImageUrl "./ship.png" (f << TextureLoaded "ship" (\envBuilder texture -> { envBuilder | shipTexture = texture }))
    , Canvas.Texture.loadFromImageUrl "./fire.png" (f << TextureLoaded "fire" (\envBuilder texture -> { envBuilder | fireTexture = texture }))
    ]


step : Random.Generator a -> Env -> ( a, Env )
step generator env =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, { env | seed = nextSeed } )


fromBuilder : Builder -> Load Env
fromBuilder builder =
    builder.shipTexture
        |> bindLoad
            (\shipTexture ->
                builder.fireTexture
                    |> bindLoad
                        (\fireTexture ->
                            case builder.seed of
                                Just seed ->
                                    Success
                                        { seed = seed
                                        , fireTexture = fireTexture
                                        , shipTexture = shipTexture
                                        , pressedKeys = builder.pressedKeys
                                        , pressedButtons = builder.pressedButtons
                                        , canvasSize = builder.canvasSize
                                        }

                                Nothing ->
                                    Loading
                        )
            )


subscriptions : Sub Msg
subscriptions =
    Sub.map KeyMsg Keyboard.subscriptions
