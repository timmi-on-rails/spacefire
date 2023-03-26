module Env exposing (Builder, Button(..), Env, Msg(..), fromBuilder, init, step, subscriptions, textures, update, updateB)

import Canvas.Texture
import Keyboard
import Mouse
import Random


type alias Builder =
    { seed : Maybe Random.Seed
    , shipTexture : Load Canvas.Texture.Texture
    , fireTexture : Load Canvas.Texture.Texture
    , canvasSize : { width : Int, height : Int }
    }


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
      , fireTexture = Loading
      , shipTexture = Loading
      , canvasSize = { width = 500, height = 600 }
      }
    , Random.generate InitSeed Random.independentSeed
    )


updateB : Msg -> Builder -> ( Builder, Cmd msg )
updateB msg builder =
    case msg of
        InitSeed s ->
            ( { builder | seed = Just s }, Cmd.none )

        TextureLoaded name u mt ->
            case mt of
                Just t ->
                    ( u builder (Success t), Cmd.none )

                Nothing ->
                    ( "failed to load texture " ++ name |> Failure |> u builder, Cmd.none )

        KeyMsg _ ->
            ( builder, Cmd.none )

        MouseMsg _ ->
            ( builder, Cmd.none )


update : Msg -> Env -> ( Env, Cmd msg )
update msg env =
    case msg of
        KeyMsg k ->
            ( { env | pressedKeys = Keyboard.update k env.pressedKeys }, Cmd.none )

        MouseMsg x ->
            ( { env | pressedButtons = Mouse.update x env.pressedButtons }, Cmd.none )

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


fromBuilder : Builder -> Result String (Maybe Env)
fromBuilder builder =
    case builder.shipTexture of
        Success shipTexture ->
            case builder.fireTexture of
                Success fireTexture ->
                    case builder.seed of
                        Just seed ->
                            { seed = seed
                            , fireTexture = fireTexture
                            , shipTexture = shipTexture
                            , pressedKeys = []
                            , pressedButtons = []
                            , canvasSize = builder.canvasSize
                            }
                                |> Just
                                |> Ok

                        Nothing ->
                            Ok Nothing

                Loading ->
                    Ok Nothing

                Failure err ->
                    Err err

        Loading ->
            Ok Nothing

        Failure err ->
            Err err


subscriptions : Sub Msg
subscriptions =
    Sub.map KeyMsg Keyboard.subscriptions
