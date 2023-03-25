module Game exposing (Button(..), Env, Game, init, render, update)

import Canvas
import Canvas.Texture
import Keyboard
import Random


fireSize : ( Float, Float )
fireSize =
    ( 25, 25 )


type alias Env =
    { seed : Random.Seed
    , pressedKeys : List Keyboard.Key
    , pressedButtons : List Button
    , shipTexture : Canvas.Texture.Texture
    , fireTexture : Canvas.Texture.Texture
    }


type alias Ship =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Game =
    { env : Env
    , ship : Ship
    , nextFireEta : Float
    , fires : List ( Float, Float )
    , missedFires : Int
    , killedFires : Int
    }


type Button
    = Left
    | Right


init : ( Int, Int ) -> Env -> Game
init ( _, h ) env =
    { env = env
    , ship =
        { x = 0
        , y = 0.8 * toFloat h
        , width = 50
        , height = 50
        }
    , nextFireEta = 0
    , fires = []
    , missedFires = 0
    , killedFires = 0
    }
        |> resetFireEta


update : ( Int, Int ) -> Float -> Game -> Game
update canvasSize delta game =
    game
        |> moveShip canvasSize delta
        |> decrementFireEta delta
        |> spawnFire canvasSize
        |> resetFireEta
        |> moveFires delta
        |> handleMissedFires canvasSize
        |> handleCollisions


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


moveShip : ( Int, Int ) -> Float -> Game -> Game
moveShip ( w, _ ) delta game =
    { game
        | ship =
            let
                ship =
                    game.ship
            in
            { ship
                | x =
                    max (-0.5 * toFloat w + 0.5 * ship.width) (min (0.5 * toFloat w - 0.5 * ship.width) (ship.x + shipVx game * delta))
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


handleMissedFires : ( Int, Int ) -> Game -> Game
handleMissedFires ( _, h ) game =
    let
        cond f =
            Tuple.second f < toFloat h

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


spawnFire : ( Int, Int ) -> Game -> Game
spawnFire ( w, _ ) game =
    if game.nextFireEta <= 0 then
        let
            g =
                Random.float (-0.5 * toFloat w) (0.5 * toFloat w)

            ( r, newEnv ) =
                step g game.env
        in
        { game | env = newEnv, fires = ( r, 0 ) :: game.fires }

    else
        game


render : ( Int, Int ) -> Game -> List Canvas.Renderable
render ( w, h ) game =
    Canvas.texture [] ( 0.5 * toFloat w + game.ship.x - 0.5 * game.ship.width, game.ship.y ) game.env.shipTexture
        :: fireShapes ( w, h ) game


fireShapes : ( Int, Int ) -> Game -> List Canvas.Renderable
fireShapes ( w, _ ) game =
    let
        ( fw, _ ) =
            fireSize
    in
    game.fires
        |> List.map (\f -> Canvas.texture [] ( 0.5 * toFloat w + Tuple.first f - 0.5 * fw, Tuple.second f ) game.env.fireTexture)


step : Random.Generator a -> Env -> ( a, Env )
step generator env =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, { env | seed = nextSeed } )
