module Main where

import Lib

{- This is the main entry point to your program. -}

import Graphics.Gloss 
import Graphics.Gloss.Interface.Pure.Game

--State data type signifies what state the application is in when ran
data State = Title | PlayGame | Gameover


fps :: Int
fps = 90


background :: Color
background =  light (light blue)

--sets the window size to be 300px by 420 px
windowDisplay :: Display
windowDisplay = InWindow "BAPPYBOCK" (300, 420) (10, 10)


--defines the record for how our game is going to be stored
--changes to the application are made through functions that interact with this record
data Flappybird = Game
  {birdHeight :: Float
  ,birdVelocity :: Float
  ,pipetop :: (Float,Float)
  ,pipebot :: (Float,Float)
  ,state :: State
  ,score :: Int
  }

--sets the game with inital values
initialState :: Flappybird
initialState = Game
  {birdHeight = 0
  ,birdVelocity =(-5)
  ,pipetop = (150,230)
  ,pipebot = (150,(-230)) 
  ,state = Title
  ,score = 0
  }

--render function calls the respective rendering functions dependant on the state the game is in done by pattern matching
--as this is the most clean way to determine the state of the game and call its respective function, also means that the main function
--doesnt need to be editted at all 
render :: Flappybird -> Picture
render game = case (state game) of
  Title -> renderTitle game
  PlayGame -> renderGame game
  Gameover -> renderGameover game

--renders the title screen with text 
renderTitle :: Flappybird -> Picture
renderTitle game =
  pictures [translate (-90) (60) $ color yellow $ scale 0.25 0.25 $ text ("BAPPYBOCK"),
  translate (-120) (20) $ color red $ scale 0.2 0.2 $ text ("SPACE TO PLAY!"),
  translate (-150) (-20) $ color green $ scale 0.15 0.15 $ text ("Touch the Ground," ),
  translate (-150) (-40) $ color green $ scale 0.15 0.15 $ text ("go to high or,"),
  translate (-150) (-60) $ color green $ scale 0.15 0.15 $ text ("touch a pipe and you lose"),
  translate (-150) (-80) $ color green $ scale 0.13 0.13 $ text ("score is obtained from each pipe"),
  translate (-150) (-100) $ color green $ scale 0.15 0.15 $ text ("that fully passes the screen"),
  translate (-150) (-120) $ color yellow $ scale 0.15 0.15 $ text ("Good Luck!")
  ]

--renders the game screen
renderGame :: Flappybird -> Picture
renderGame game = 
  --renders all the "sprites" of the game by using the pictures function we can render a list of pictures at once
  pictures [bird, pipeTop , pipeBot,grass, ground]
    where
      bird = translate (-50) (birdHeight game) $ color birdColor $ rectangleSolid 20 15
      birdColor = dark yellow 
      pipeTop = uncurry translate (pipetop game) $ color pipeColor $ rectangleSolid 30 420
      pipeColor = light (light green)
      pipeBot = uncurry translate (pipebot game) $ color pipeColor $ rectangleSolid 30 420
      grass = translate 0 (-210) $ color grassColor $ rectangleSolid 1000 70
      grassColor = dark (dark green)
      ground = translate 0 (-210) $ color groundColor $ rectangleSolid 1000 50
      groundColor = dark (dark orange)

--renders the game over screen
renderGameover :: Flappybird -> Picture
renderGameover game = 
  pictures[translate (-150) (20) $ color red $ scale 0.2 0.2 $ text ("SPACE TO RESTART"),
   translate (-150) (0) $ color green $ scale 0.2 0.2 $ text ("You Got" ++ show(score game))
  ]

-- function which controls the movement of the bird (Rectangle) and pipes
moveBird :: Float -> Flappybird -> Flappybird
moveBird seconds game = game {birdHeight = y' , birdVelocity = z' , pipetop = a , pipebot = b, state = c, score = d}
  where
    x = birdHeight game
    v = birdVelocity game
    -- applies gravity to the birds height, making it go down 
    y' = x + v * seconds
    -- bird accelerates downwards as it doesn't jump
    z' = birdVelocity game - 2.2
    -- helper which checks and changes the location of the top pipe ,  if at end remake with another pipe
    a = (f (fst (pipetop game)) , gennew (snd (pipetop game)))
      where
        f :: Float -> Float
        f a = if a == (-165) 
              then (165)
              else a - 1
        gennew a = if fst (pipetop game) == (-165)
                then (helper (a + 4325432)) -- adds an element of randomness/a variation of heights which the pipe gaps are
                else a
    b = (g (fst (pipebot game)) , gennew2 (snd (pipebot game)))
      where
        g :: Float -> Float
        g a = if a == (-165) 
              then (165)
              else a - 1
        gennew2 a = if fst (pipebot game) == (-165)
                then (helper (snd (pipetop game) + 4325432) - 460) --subtracts 460 as this is the gap between the top of the gap and bottom
                else a
    -- detects if there have been collisions with the pipe, floor or height limit
    -- if there has been a collision the game should end
    c = collisiondetect (pipetop game) (pipebot game)
      where
        collisiondetect :: (Float,Float) -> (Float,Float) -> State
        collisiondetect (a,b) (c,d)
          | a>=(-75) && a<=(-25) = if birdHeight game >= (b - 217.5) || birdHeight game <= (d + 217.5) 
                                      then Gameover --SET THIS TO PlayGame FOR GOD MODE FROM PIPES (FOR TESTING)
                                      else PlayGame
          | birdHeight game < (-167.5) = Gameover 
          | birdHeight game > 210 = Gameover
          | otherwise = PlayGame
    --increments the score counter
    d = if fst (pipetop game) == (-165) 
        then (score game) + 1
        else score game
-- the update function which is ran if the user is on a title or gameover screen
nothingfunc :: Float -> Flappybird -> Flappybird
--if on the gameover screen as opposed to resetting the pipes to have the same height pattern, the sequence continues from where it left off 
nothingfunc seconds game = game {birdHeight = 0 ,birdVelocity =(-5) ,pipetop = (150,snd (pipetop game)) ,pipebot = (150,(snd (pipebot game)))}

--runs the application
main :: IO ()
main = play windowDisplay background fps initialState render handlekeys update

--does pattern matching to determine which update function to use dependant on what state the game is in
update :: Float -> Flappybird -> Flappybird
update seconds game = case state game of
  PlayGame -> moveBird seconds game
  _ -> nothingfunc seconds game

--decides what certain key presses do to the application dependant on the state of the game by pattern matching
handlekeys :: Event -> Flappybird -> Flappybird
handlekeys (EventKey (SpecialKey KeySpace) _ _ _) game = case state game of
  -- makes the bird jump 
  Title -> game {birdVelocity = 100 , state = PlayGame}
  PlayGame -> game {birdVelocity = 100}
  --resets the bird position and score
  Gameover -> game {birdHeight = 0 ,state = PlayGame, score = 0}
handlekeys _ game = game

--helper function that does a basic math calculation to help set the new height of the pipe gap
helper :: Float -> Float
helper a = if a > 410
          then helper (a - 247)
          else a
