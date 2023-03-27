module Game exposing
    ( Game
    , getEnv
    , init
    , render
    , setEnv
    , update
    )

import Canvas
import Env exposing (Env)
import Keyboard
import Random


fireSize : ( Float, Float )
fireSize =
    ( 25, 25 )


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


init : Env -> Game
init env =
    Game
        { env = env
        , ship =
            { x = 0
            , y = 0.8 * toFloat (env |> Env.canvasSize |> .height)
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


setEnv : Game -> Env -> Game
setEnv (Game game) env =
    Game { game | env = env }


shipVx : Game -> Float
shipVx (Game game) =
    List.sum
        [ if
            (Env.pressedKeys game.env |> List.member Keyboard.ArrowLeft)
                || (Env.pressedButtons game.env |> List.member Env.Left)
          then
            -0.3

          else
            0
        , if
            (Env.pressedKeys game.env |> List.member Keyboard.ArrowRight)
                || (Env.pressedButtons game.env |> List.member Env.Right)
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
                        game.env |> Env.canvasSize
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
                Env.step g game.env
        in
        Game { game | env = newEnv, nextFireEta = r }

    else
        Game game


handleMissedFires : Game -> Game
handleMissedFires (Game game) =
    let
        cond f =
            Tuple.second f < toFloat (game.env |> Env.canvasSize |> .height)

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
                game.env |> Env.canvasSize

            g =
                Random.float (-0.5 * toFloat width) (0.5 * toFloat width)

            ( r, newEnv ) =
                Env.step g game.env
        in
        Game { game | env = newEnv, fires = ( r, 0 ) :: game.fires }

    else
        Game game


render : Game -> List Canvas.Renderable
render (Game game) =
    let
        { width } =
            game.env |> Env.canvasSize
    in
    Canvas.texture [] ( 0.5 * toFloat width + game.ship.x - 0.5 * game.ship.width, game.ship.y ) (Env.textures game.env |> .shipTexture)
        :: fireShapes (Game game)


fireShapes : Game -> List Canvas.Renderable
fireShapes (Game game) =
    let
        ( fw, _ ) =
            fireSize

        { width } =
            game.env |> Env.canvasSize
    in
    game.fires
        |> List.map (\f -> Canvas.texture [] ( 0.5 * toFloat width + Tuple.first f - 0.5 * fw, Tuple.second f ) (Env.textures game.env |> .fireTexture))
