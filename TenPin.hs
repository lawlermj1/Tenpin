{- |
   Module      : TenPin 
   Description : TenPin scoring functions
   Copyright   : ( c ) Matthew Lawler 2018 
   License     : 2-Clause BSD
   Maintainer  : lawlermj1@gmail.com

   This module defines functions used to score Ten Pin. 
   Assume that input is a Frames list. 
   Scoring will include spares and strikes, but ignore exclude fouls and splits. 
    
 -}
module TenPin
    (  
--    PUBLIC Types: input
      Frames( .. ), 

      frames2ScoreDirect, 
      scoreDefault, 
      inValidFrames, 
      areAllFramesOK, 
      frames2ScoreMap, 
      frames2ScoreFold, 
      
      prop_frames2ScoreMap_eq_frames2ScoreFold,
      prop_frames2ScoreMap_eq_frames2ScoreFold_filter_Wrong,
      prop_frames2ScoreMap_eq_frames2ScoreFold_filter_OK,

     ) where
--------------------------------------------------------------------------------

import Data.Either 

type Frame = (Int,Int) 
type Frames = [Frame]  

data Score = Score { 
          scoreFrames :: Frames 
        , scoreValues :: [Int] 
        , scoreRunningTotal :: [Int] 
        , scoreTotal :: Int 
                 } deriving (Ord, Eq) 
--    very simple show intercalate "\n"
instance Show Score where
    show (Score sf sv srt st) = "\n Frames = " ++ show sf ++ "\n Values = " ++ show sv ++ "\n Run Total = " ++ show srt ++ " Total = " ++ show st  
scoreDefault = Score [] [] [] 0 

----   Input Error checking 

--    checks if frame pins obey tenpin rules 
isValidFrame :: Frame -> Either String Frame 
isValidFrame (t1, t2) 
--    negative throws not allowed and limit of 10 pins 
    | t1 < 0 = Left ("Throw 1 pins cannot be negative: t1 = " ++ show t1)  
    | t1 > 10 = Left ("Throw 1 pins cannot be greater than 10: t1 = " ++ show t1) 
    | t2 < 0 = Left ("Throw 2 pins cannot be negative: t2 = " ++ show t2) 
    | t2 > 10 = Left ("Throw 2 pins cannot be greater than 10: t2 = " ++ show t2) 
--     cannot knock over more than 10 pins, if not a first strike 
    | t1 < 10 && t1 + t2 > 10 = Left ("If not a first strike, throw 1 and 2 pins total cannot be greater than 10: t1 = " ++ show t1 ++ " t2 = " ++ show t2) 
--    if strike, no second throw 
    | t1 == 10 && t2 > 0 = Left ("If a first strike, throw 2 must be zero: t1 = " ++ show t1 ++ " t2 = " ++ show t2) 
    | otherwise = Right (t1, t2) 
    
inValidFrames :: Frames -> [String] 
inValidFrames fs = lefts (map isValidFrame fs) 
    
--    checks if frame pins obey tenpin rules 
areAllFramesOK :: Frames -> Either String Frames  
areAllFramesOK fs 
--    but ok if perfect 
    | length fs > 10 = Left ("No, too many frames. max is 10. number = " ++ show (length fs)) 
    | frames2ScoreMap fs > 300 = Left ("No, total score too high. max is 300. total = " ++ show (frames2ScoreMap fs)) 
    | otherwise = Right fs

---    Summing the frames 
isStrike :: Frame -> Bool 
isStrike = (== 10).fst 

notStrike :: Frame -> Bool 
notStrike = (< 10).fst 

isSpare :: Frame -> Bool 
isSpare f = notStrike f && fst f + snd f == 10 

--    simplest case 
sumFrame :: Frame -> Int 
sumFrame f = fst f + snd f 

--    scoring rules Arrgh!!! 
--    assumes that ninth strike, ... 
--    perfect game with 2 more strikes... 
--    if tenth frame is a strike, then 11th and 12th throw happen 
--    if tenth frame is a spare, then 11th throw happens 

--  scoring the first frame or f1 only 
sumFrames :: Frames -> Int 
--    empty frames  
sumFrames [] = 0 
--    single frame case     
sumFrames [f1] 
    | otherwise = sumFrame f1 
--    2 frame case 
sumFrames (f1:f2:[]) 
--    2 frame; 1 strike and a non-strike 
    | isStrike f1 && notStrike f2 = sumFrame f1 + sumFrame f2 
--   2 frame; 1 spare and whatever 
    | isSpare f1 = sumFrame f1 + fst f2 
    | otherwise = sumFrame f1    
--    3 or more frame case 
sumFrames (f1:f2:f3:_) 
--    3 frame; 3 strikes 
    | isStrike f1 && isStrike f2 && isStrike f3 = sumFrame f1 + sumFrame f2 + sumFrame f3 
--    3 frame; 2 strikes and a non-strike 
    | isStrike f1 && isStrike f2 && notStrike f3 = sumFrame f1 + sumFrame f2 + fst f3 
--    2 frame; 1 strike and a non-strike 
    | isStrike f1 && notStrike f2  = sumFrame f1 + sumFrame f2     
--    2 frame; 1 spare and whatever 
    | isSpare f1 = sumFrame f1 + fst f2 
    | otherwise = sumFrame f1 

--    two distinct scoring functions means that they can be used in the properties below   
--    direct recursion example 
frames2ScoreDirect :: Score -> Frames -> Score 
--    base value + take first 10 frames only only 
frames2ScoreDirect ss [] = ss { scoreTotal = sum (take 10 (scoreValues ss)) } 
--    recursion 
frames2ScoreDirect ss (f:fs) 
    = frames2ScoreDirect 
        (ss { 
--    add the frame for reference 
            scoreFrames = (scoreFrames ss) ++ [f], 
--    calc value for this frame 
            scoreValues = (scoreValues ss) ++ [sumFrames (f:fs)],  
--    get running total of frames 
            scoreRunningTotal = (scoreRunningTotal ss) ++ [sum ((scoreValues ss) ++ [sumFrames (f:fs)])]  
            }) fs 

--    recursion using map  
--    note that this uses an overly simplistic scoring function 
frames2ScoreMap :: Frames -> Int  
frames2ScoreMap fs = sum (map sumFrame fs)   

--    point free style 
frames2ScoreFold :: Frames -> Int  
frames2ScoreFold = foldl (\acc f -> acc + sumFrame f) 0 

--    property function equivalence 
prop_frames2ScoreMap_eq_frames2ScoreFold :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreFold fs = frames2ScoreMap fs == frames2ScoreFold fs  

--    property function equivalence 
--    this is deliberately broken, so it will always fail quickCheck 
prop_frames2ScoreMap_eq_frames2ScoreFold_filter_Wrong :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreFold_filter_Wrong fs = frames2ScoreMap (filter (isRight.isValidFrame) fs) == frames2ScoreFold fs  

--    property function equivalence 
prop_frames2ScoreMap_eq_frames2ScoreFold_filter_OK :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreFold_filter_OK fs 
    = frames2ScoreMap gs == frames2ScoreFold gs   
    where gs = (filter (isRight.isValidFrame) fs) 

