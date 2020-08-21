
module Lib where

import Numeric
import Numeric.Extra

gpxFor :: String -> String
gpxFor given = toGPX $ foldl doStep [start] (zip questions given)

doStepIO :: [State] -> (String,Char) -> IO [State]
doStepIO oldStates (description, c) = do
  putStrLn $ description ++ " " ++ [c] ++ " means " ++ show (navigate c)
  let newS = move (last oldStates) (navigate c)
  print newS
  return $ oldStates ++ [newS]

doStep :: [State] -> (String,Char) -> [State]
doStep oldStates (_, c) = let newS = move (last oldStates) (navigate c) in oldStates ++ [newS]

questions :: [String]
questions =
  -- Etappe I:
  [ "I   1"
  , "I   2"
  , "I   3"
  , "I   4"
  , "I   5"
  , "I   6"
  , "I   7"
  , "I   8"
  , "I   9"
  -- Etappe II:
  , "II  1"
  , "II  2"
  , "II  3"
  , "II  4"
  , "II  5"
  , "II  6"
  , "II  7"
  , "II  8"
  , "II  9"
  -- Etappe III:
  , "III 1"
  , "III 2"
  , "III 3"
  , "III 4"
  , "III 5"
  , "III 6"
  , "III 7"
  , "III 8"
  , "III 9"
  -- Etappe IV:
  , "IV  1"
  , "IV  2"
  , "IV  3"
  , "IV  4"
  , "IV  5"
  , "IV  6"
  , "IV  7"
  , "IV  8"
  , "IV 9?"
  ]

-- | State (N-S) (E-W) Heading
data State = State Latitude Longitude Heading
type Latitude = Rational
type Longitude = Rational
data Heading = North | South | East | West | NotGiven deriving (Show)

instance Show State where
  show (State lat lon curHead) =
    "State " ++ show (fromRat lat :: Double) ++ " " ++ show (fromRat lon :: Double) ++ " " ++ show curHead

type Move = (Direction,Distance)
data Direction = N | S | E | W | Straight deriving (Show)
data Distance = Deg Int | Mil Int deriving (Show)

headToDir :: Heading -> Direction
headToDir North = N
headToDir South = S
headToDir East  = E
headToDir West  = W
headToDir NotGiven = error "no initial heading given"

start :: State
start = State 0 0 NotGiven

move :: State -> Move -> State
move s@(State curLat curLon curHead) theMove = fixCoord $ case theMove of
  (Straight, Deg d) -> move s (headToDir curHead, Deg d)
  (Straight, Mil _) -> error "this is never used."
  (N, Deg d) -> State (curLat + toRational d) (curLon)     North
  (S, Deg d) -> State (curLat - toRational d) curLon       South
  (E, Deg d) -> State curLat       (curLon + toRational d) East
  (W, Deg d) -> State curLat       (curLon - toRational d) West
  -- Am Äquator ist eine Seemeile 1/60 Grad
  (N, Mil m) -> State (curLat + (toRational m / 60)) curLon North
  (S, Mil 23) -> State (curLat - (toRational (23::Int) / 60)) (curLon - 28) West -- special move for 7 and M
  (S, Mil m) -> State (curLat - (toRational m / 60)) curLon West
  (E, Mil _) -> error "this is never used"
  (W, Mil _) -> error "this is never used"

-- Navigationssystem -- seehttps://bit.ly/3gGOLJ6
navigate :: Char -> Move
navigate '1' = (N, Deg 40)
navigate 'A' = (N, Deg 40)
navigate '2' = (Straight, Deg 23)
navigate 'G' = (Straight, Deg 23)
navigate '3' = (E, Deg 39)
navigate 'H' = (E, Deg 39)
navigate 'I' = (E, Deg 39)
navigate '4' = (W, Deg 3)
navigate 'R' = (W, Deg 3)
navigate 'T' = (W, Deg 3)
navigate '5' = (W, Deg 28)
navigate 'U' = (W, Deg 28)
navigate '6' = (S, Deg 39)
navigate 'V' = (S, Deg 39)
navigate '7' = (S, Mil 23) -- also (W, Deg 28) done in move function!
navigate 'M' = (S, Mil 23) -- also (W, Deg 28) done in move function!
navigate '8' = (N, Mil 23)
navigate 'W' = (N, Mil 23)
navigate '9' = (Straight, Deg 2)
navigate 'E' = (Straight, Deg 2)
navigate '0' = (S, Deg 72)
navigate 'B' = (S, Deg 72)
navigate 'C' = (S, Deg 72)
navigate 'S' = (S, Deg 72)
navigate 'J' = (S, Deg 47)
navigate 'K' = (S, Deg 47)
navigate 'F' = (E, Deg 6)
navigate 'N' = (E, Deg 6)
navigate 'X' = (N, Deg 12)
navigate 'Y' = (N, Deg 12)
navigate 'L' = (N, Deg 10)
navigate 'O' = (N, Deg 10)
navigate 'P' = (W, Deg 70)
navigate 'Q' = (W, Deg 70)
navigate 'D' = (E, Deg 74)
navigate 'Z' = (E, Deg 74)
navigate c   = error $ c : " is not in nvigation system"

-- correctly walk over the North pole:
fixCoord :: State -> State
fixCoord (State lat lon North) | lat > 90 && lon >  0 = (State (180 - lat) (lon - 180) South)
                               | lat > 90 && lon <= 0 = (State (180 - lat) (lon + 180) South)
                               | otherwise            = State lat lon North
fixCoord s = s

-- TODO merge fixCoord and showLon?

showLon :: Rational -> String
showLon lon = showDP 6 (fromRational corrected :: Double) where
  corrected = if lon > 180 then lon - 360 else lon

showLat :: Rational -> String
showLat lat = showDP 6 (fromRational lat :: Double)

toGPX :: [State] -> String
toGPX states = prelude ++ waypoints ++ trackpoints ++ footer where
  prelude ="<?xml version=\"1.0\" encoding=\"UTF-8\"?><gpx version=\"1.0\">\n<name>ZEIT</name>\n"
  trackpoints =
    "<trk>\n<name>My Track</name><number>1</number>\n<trkseg>\n"
    ++
    concat [ "<trkpt lat=\""++ showLat lat ++ "\" lon=\""++ showLon lon ++ "\"></trkpt>\n"
           | (State lat lon _) <- states ]
    ++ "</trkseg></trk>"
  waypoints = concat [ concat [ "<wpt lat=\"", showLat lat, "\" lon=\"", showLon lon, "\">"
                              , "<name>", desc, "</name>"
                              , "<desc>", showLat lat, ",", showLon lon, "</desc>"
                              , "</wpt>\n"
                              ]
                     | (desc, State lat lon _) <- zip ("0" : questions) states ]
  footer = "</gpx>"
