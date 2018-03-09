{-# LANGUAGE FlexibleContexts #-} 

--    assume that input is Frames list 
--    include spare, strike 
--    exclude fouls, splits 

import Data.Either 
import Data.List  
import Test.QuickCheck 

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


main = do 
  
    let perfectGame = [(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0),(10,0)] 
--    examples from wiki page 
    let doublePinfall = [(10,0),(10,0),(9,0)] 
    let turkeyPinfall = [(10,0),(10,0),(10,0),(8,2),(8,1)]   
    let sparePinfall = [(7,3),(4,2)] 
--    example from https://www.thoughtco.com/bowling-scoring-420895  
    let game1 = [(10,0),(7,3),(7,2),(9,1),(10,0),(10,0),(10,0),(2,3),(6,4),(7,3),(3,0)] 

--   Scoring 
    putStrLn $ "\nScoring \n "
    putStrLn $ "frames2ScoreDirect perfectGame = exp 300 " ++ show ( frames2ScoreDirect scoreDefault perfectGame ) ++ "\n" 

    putStrLn $ "frames2ScoreDirect doublePinfall = exp 57 " ++ show ( frames2ScoreDirect scoreDefault doublePinfall ) ++ "\n" 

    putStrLn $ "frames2ScoreDirect turkeyPinfall = exp 105 " ++ show ( frames2ScoreDirect scoreDefault turkeyPinfall ) ++ "\n" 

    putStrLn $ "frames2ScoreDirect sparePinfall = exp 20 " ++ show ( frames2ScoreDirect scoreDefault sparePinfall ) ++ "\n" 
    
    putStrLn $ "frames2ScoreDirect game1 = exp 168 " ++ show ( frames2ScoreDirect scoreDefault game1 ) ++ "\n" 
    
--    Input error checks 
    putStrLn $ "\nInput Checks \n "
    let badFrames = [(-1,-2),(4,-5),(11,-3),(1,11),(10,9),(8,8),(7,7),(6,6),(5,5),(1,9),(2,8),(3,7)]      
    
    putStrLn $ "frames2ScoreDirect badFrames = " ++ show ( frames2ScoreDirect scoreDefault badFrames ) ++ "\n" 
    
    putStrLn $ "inValidFrames = " ++ "\n" ++ intercalate "\n" ( inValidFrames badFrames ) ++ "\n" 
    
    putStrLn $ "areAllFramesOK = " ++ (fromLeft "Yes" (areAllFramesOK badFrames) ) ++ "\n"  

--    property based testing 
--    note these are invalid, as they use an overly simple scoring algorithm 
--    they are included to demo how this could be done with more work 
    putStrLn $ "\nProperty Tests \n "
    
    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreFold = " ++ show ( prop_frames2ScoreMap_eq_frames2ScoreFold badFrames ) ++ "\n" 
    
    putStrLn $ "frames2ScoreMap = " ++ show ( frames2ScoreMap badFrames ) ++ "\n" 
    putStrLn $ "frames2ScoreFold = " ++ show ( frames2ScoreFold badFrames ) ++ "\n" 

    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreFold_filter_OK = "
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreFold_filter_OK  
    
    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreFold = " 
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreFold  
    
    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreFold_filter_Wrong = " 
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreFold_filter_Wrong  

    putStrLn $ "badFrames = "     
    
    mapM_ (putStrLn.show) badFrames 
    
--------------------------
--    Not used
--    included for discussion only 
    
-- function equality 
fnEq :: Eq (a -> b) => (a -> b) -> (a -> b) -> Bool 
fnEq f g = f == g 
    
--    function equality property  
--    But error: 
--    No instance for (Eq (Frames -> Bool)) 
prop_frames2ScoreMap_eq_frames2ScoreFold_fn :: (Frames -> Bool) -> (Frames -> Bool) -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreFold_fn frames2ScoreMap frames2ScoreFold
--    = fnEq frames2ScoreMap frames2ScoreFold 
    = undefined     
    
