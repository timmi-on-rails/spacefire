module Env exposing (Button(..), Env, step)

import Canvas.Texture
import Keyboard
import Random


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
