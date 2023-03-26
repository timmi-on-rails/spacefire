module Env exposing (Builder, Button(..), Env, fromBuilder, step)

import Canvas.Texture
import Keyboard
import Random


type alias Builder =
    { seed : Maybe Random.Seed
    , shipTexture : Maybe Canvas.Texture.Texture
    , fireTexture : Maybe Canvas.Texture.Texture
    , canvasSize : { width : Int, height : Int }
    }


type alias Env =
    { seed : Random.Seed
    , pressedKeys : List Keyboard.Key
    , pressedButtons : List Button
    , shipTexture : Canvas.Texture.Texture
    , fireTexture : Canvas.Texture.Texture
    , canvasSize : { width : Int, height : Int }
    }


type Button
    = Left
    | Right


step : Random.Generator a -> Env -> ( a, Env )
step generator env =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, { env | seed = nextSeed } )


fromBuilder : Builder -> Maybe Env
fromBuilder builder =
    case builder.shipTexture of
        Just shipTexture ->
            case builder.fireTexture of
                Just fireTexture ->
                    case builder.seed of
                        Just seed ->
                            Just
                                { seed = seed
                                , fireTexture = fireTexture
                                , shipTexture = shipTexture
                                , pressedKeys = []
                                , pressedButtons = []
                                , canvasSize = builder.canvasSize
                                }

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing
