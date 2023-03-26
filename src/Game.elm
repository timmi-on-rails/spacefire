module Game exposing (Button(..), Env, Game, getEnv, init, render, update, updateEnv)

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
    , canvasSize : { width : Int, height : Int }
    }


type alias Ship =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type Game
    = Game
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


init : Env -> Game
init env =
    Game
        { env = env
        , ship =
            { x = 0
            , y = 0.8 * toFloat env.canvasSize.height
            , width = 50
            , height = 50
            }
        , nextFireEta = 0
        , fires = []
        , missedFires = 0
        , killedFires = 0
        }
        |> resetFireEta


update : Float -> Game -> Game
update delta game =
    game
        |> moveShip delta
        |> decrementFireEta delta
        |> spawnFire
        |> resetFireEta
        |> moveFires delta
        |> handleMissedFires
        |> handleCollisions


getEnv : Game -> Env
getEnv (Game game) =
    game.env


updateEnv : (Env -> Env) -> Game -> Game
updateEnv f (Game game) =
    Game { game | env = f game.env }


shipVx : Game -> Float
shipVx (Game game) =
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
moveFires delta (Game game) =
    let
        move : ( Float, Float ) -> ( Float, Float )
        move f =
            ( Tuple.first f, Tuple.second f + 0.1 * delta )

        newFires =
            game.fires |> List.map move
    in
    Game { game | fires = newFires }


moveShip : Float -> Game -> Game
moveShip delta (Game game) =
    Game
        { game
            | ship =
                let
                    ship =
                        game.ship

                    { width } =
                        game.env.canvasSize
                in
                { ship
                    | x =
                        max (-0.5 * toFloat width + 0.5 * ship.width) (min (0.5 * toFloat width - 0.5 * ship.width) (ship.x + shipVx (Game game) * delta))
                }
        }


resetFireEta : Game -> Game
resetFireEta (Game game) =
    if game.nextFireEta <= 0 then
        let
            g =
                Random.float 1000 5000

            ( r, newEnv ) =
                step g game.env
        in
        Game { game | env = newEnv, nextFireEta = r }

    else
        Game game


handleMissedFires : Game -> Game
handleMissedFires (Game game) =
    let
        cond f =
            Tuple.second f < toFloat game.env.canvasSize.height

        newFires =
            game.fires |> List.filter cond
    in
    Game { game | fires = newFires, missedFires = List.length game.fires - List.length newFires }


handleCollisions : Game -> Game
handleCollisions (Game game) =
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
    Game { game | fires = newFires, killedFires = List.length game.fires - List.length newFires }


decrementFireEta : Float -> Game -> Game
decrementFireEta delta (Game game) =
    Game { game | nextFireEta = game.nextFireEta - delta }


spawnFire : Game -> Game
spawnFire (Game game) =
    if game.nextFireEta <= 0 then
        let
            { width } =
                game.env.canvasSize

            g =
                Random.float (-0.5 * toFloat width) (0.5 * toFloat width)

            ( r, newEnv ) =
                step g game.env
        in
        Game { game | env = newEnv, fires = ( r, 0 ) :: game.fires }

    else
        Game game


render : Game -> List Canvas.Renderable
render (Game game) =
    let
        { width } =
            game.env.canvasSize
    in
    Canvas.texture [] ( 0.5 * toFloat width + game.ship.x - 0.5 * game.ship.width, game.ship.y ) game.env.shipTexture
        :: fireShapes (Game game)


fireShapes : Game -> List Canvas.Renderable
fireShapes (Game game) =
    let
        ( fw, _ ) =
            fireSize

        { width } =
            game.env.canvasSize
    in
    game.fires
        |> List.map (\f -> Canvas.texture [] ( 0.5 * toFloat width + Tuple.first f - 0.5 * fw, Tuple.second f ) game.env.fireTexture)


step : Random.Generator a -> Env -> ( a, Env )
step generator env =
    let
        ( a, nextSeed ) =
            env.seed |> Random.step generator
    in
    ( a, { env | seed = nextSeed } )
