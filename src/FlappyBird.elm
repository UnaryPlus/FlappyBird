{-
  (c) 2022 Owen Bechtel
  License: MIT (see LICENSE file)  
-}

module FlappyBird exposing (main)

import Playground exposing (..)

birdSrc = "bird.png"
birdWidth = 56.66
birdHeight = 40

obstacleWidth = 85
gapHeight = 150

type alias Bird =
    { x : Number
    , y : Number
    , vy : Number
    }

type alias Memory =
    { bird : Bird
    , score : Int
    , highScore : Int
    , gapY : Number
    }

leftSide : Screen -> Number
leftSide screen = max screen.left -400

rightSide : Screen -> Number
rightSide screen = min screen.right 400

randomGapY : Computer -> Number
randomGapY computer =
    let
        height = computer.screen.height - gapHeight + 1
        digit = zigzag 0 10 pi computer.time |> floor
    in
    toFloat digit * height / 9 - height / 2

init : Int -> Memory
init highScore =
    { bird = Bird -300 0 0
    , score = 0
    , highScore = highScore
    , gapY = 0
    }

view : Computer -> Memory -> List Shape
view computer memory =
    [ --obstacle
      rectangle darkGreen (obstacleWidth - 10) computer.screen.height
    , group
        [ rectangle black (obstacleWidth - 10) (gapHeight + 55)
        , rectangle darkGreen obstacleWidth (gapHeight + 50)
        , rectangle white (obstacleWidth + 1) gapHeight
        ]
        |> moveY memory.gapY

      --score text
    , String.fromInt memory.score
        |> words black
        |> scale 3
        |> move (leftSide computer.screen + 50) (computer.screen.top - 50)

    --high score text
    , String.fromInt memory.highScore
        |> words darkGray
        |> scale 3
        |> move (leftSide computer.screen + 50) (computer.screen.top - 110)

       --bird
    , image birdWidth birdHeight birdSrc
        |> move memory.bird.x memory.bird.y
    ]

update : Computer -> Memory -> Memory
update computer memory =
    let
        offscreen = memory.bird.x > rightSide computer.screen

        vy =
            if memory.bird.y == computer.screen.top - birdHeight / 2
                then -0.1
            else if computer.keyboard.up
                || computer.keyboard.space
                || computer.mouse.click
                then 5
            else memory.bird.vy - 0.3

        y =
            memory.bird.y + vy
                |> clamp
                    (computer.screen.bottom + birdHeight / 2)
                    (computer.screen.top - birdHeight / 2)

        x = if offscreen then leftSide computer.screen else memory.bird.x + 4
        score = if offscreen then memory.score + 1 else memory.score
        gapY = if offscreen then randomGapY computer else memory.gapY

        birdBottom = y - birdHeight / 2
        birdTop = y + birdHeight / 2
        birdLeft = x - birdHeight / 2
        birdRight = x + birdHeight / 2

        gapBottom = gapY - gapHeight / 2
        gapTop = gapY + gapHeight / 2
        obstacleLeft = 0 - obstacleWidth / 2
        obstacleRight = obstacleWidth / 2

        safe = birdBottom > gapBottom && birdTop < gapTop
        dead = birdRight > obstacleLeft && birdLeft < obstacleRight && not safe
    in
    if dead then init (max score memory.highScore)
    else
        { bird = Bird x y vy
        , score = score
        , highScore = memory.highScore
        , gapY = gapY
        }

main = game view update (init 0)
