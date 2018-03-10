
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
    
--    checks if frames set obeys tenpin rules 
areAllFramesOK :: Frames -> Either String Frames  
areAllFramesOK fs 
--    but ok if perfect 
    | lfs > 12 = Left ( "No, too many frames. max is 10. Frames = " ++ show fs )  
--    must be strike on 10th and 11th to have 12 frames 
--    (!!) :: [a] -> Int -> a is safe, because length determined first 
    | lfs == 12 && not ( isStrike tenth && isStrike eleventh ) = Left ( "No, 12 frames, and 10 and 11 are not strikes. Frames = " ++ show fs )
--    must be spare or strike on 10th to have 11 frames 
    | lfs == 11 && notSpare tenth = Left ( "No, 11 frames, and 10 is not a strike or a spare. Frames = " ++ show fs ) 
--    score too high 
    | frames2ScoreMap fs > 300 = Left ("No, total score too high. max is 300. total = " ++ show (frames2ScoreMap fs)) 
    | otherwise = Right fs 
    where 
        lfs = length fs 
        tenth = fs !! 9 
        eleventh = fs !! 10 

---    Summing the frames 
isStrike :: Frame -> Bool 
isStrike = (== 10).fst 

notStrike :: Frame -> Bool 
notStrike = (< 10).fst 

isSpare :: Frame -> Bool 
isSpare f = notStrike f && fst f + snd f == 10 

notSpare :: Frame -> Bool 
notSpare f = fst f + snd f < 10 

--    simplest case 
sumFrame :: Frame -> Int 
sumFrame f = fst f + snd f 

--    scoring rules Arrgh!!! 
--    assumes that ninth strike, ... 
--    perfect game with 2 more strikes... 
--    if tenth frame is a strike, then 11th and 12th throw happen 
--    if tenth frame is a spare, then 11th throw happens 
--  scoring rules using a frame triplet 
sumFramesTriple :: ( Frame, Frame, Frame ) -> Int 
--    3 frame case 
sumFramesTriple ( f1, f2, f3) 
--    3 frame; 3 strikes 
    | isStrike f1 && isStrike f2 && isStrike f3 = sumFrame f1 + sumFrame f2 + sumFrame f3 
--    3 frame; 2 strikes and a non-strike 
    | isStrike f1 && isStrike f2 && notStrike f3 = sumFrame f1 + sumFrame f2 + fst f3 
--    2 frame; 1 strike and a non-strike 
    | isStrike f1 && notStrike f2  = sumFrame f1 + sumFrame f2     
--    2 frame; 1 spare and whatever 
    | isSpare f1 = sumFrame f1 + fst f2 
--    no bonus, so take current frame only 
    | otherwise = sumFrame f1 
 
--    two distinct scoring functions means that they can be used in the properties below    
--  EXPLICIT Recursion; this is needed for explicit recursion below 
--    using the raw frames list as input 
sumFrames :: Frames -> Int     
--    empty frames  
sumFrames [] = 0 
--    single frame case     
sumFrames [f1] = sumFrame f1 
--    2 frame case 
sumFrames (f1:f2:[]) = sumFramesTriple ( f1, f2, (0,0) )   
--    3 or more frame case 
sumFrames (f1:f2:f3:_) = sumFramesTriple ( f1, f2, f3 )      

--    explicit recursion example 
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

--    MAP Recursion 
--     this provides an alternate method to invoke the scoring rules. 
--    this will then be used in the property testing below.  
--    add dummy Frames to make full length of 12 
fillFrames :: Frames -> Frames 
fillFrames fs 
    = fs ++ replicate len (0,0) 
    where 
        len = if 12 - (length fs) < 0 then 0 else 12 - (length fs) 

--    convert list into list of triples 
list2Triples :: [a] -> [( a, a, a )] 
list2Triples (x:y:z:ws) = [( x, y, z )] ++ list2Triples (y:z:ws) 
list2Triples (x:y:[]) = [] 

--    fill out frames to 12, convert to triples, then score each triple, and sum 
frames2ScoreMap :: Frames -> Int  
frames2ScoreMap fs = sum (map sumFramesTriple ((list2Triples.fillFrames) fs))  

--    fill out frames to 12, convert to triples, then score each triple, and sum 
frames2ScoreTypeMap :: Frames -> Score   
frames2ScoreTypeMap fs 
    = scoreDefault { 
            scoreFrames = fs, 
--    calc value for all frames  
            scoreValues = svs, 
--    get running total of frames 
            scoreRunningTotal = scanl1 (+) svs, 
--    final score 
            scoreTotal = sum svs 
            } 
        where svs = map sumFramesTriple ((list2Triples.fillFrames) fs)   

--    property function equivalence with no filtering except length  
prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter fs = frames2ScoreMap gs == scoreTotal ( frames2ScoreDirect scoreDefault gs ) 
    where gs = take 10 fs 

--    property function equivalence 
--    this is deliberately broken, so it will always fail quickCheck 
prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_Wrong :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_Wrong fs = frames2ScoreMap (filter (isRight.isValidFrame) fs) == scoreTotal ( frames2ScoreDirect scoreDefault fs )  

--    property function equivalence with filtering for valid frame values 
prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_OK :: Frames -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_OK fs 
    = frames2ScoreMap gs == scoreTotal (frames2ScoreDirect scoreDefault gs)    
    where gs = fromRight [] (areAllFramesOK (filter (isRight.isValidFrame) fs))  


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
    putStrLn $ "frames2ScoreMap perfectGame; exp = 300; act = " ++ show ( frames2ScoreMap perfectGame ) ++ "\n" 
    putStrLn $ "frames2ScoreTypeMap perfectGame; exp = 300; act = " ++ show ( frames2ScoreTypeMap perfectGame ) ++ "\n" 

    putStrLn $ "frames2ScoreDirect doublePinfall = exp 57 " ++ show ( frames2ScoreDirect scoreDefault doublePinfall ) ++ "\n" 
    putStrLn $ "frames2ScoreMap doublePinfall; exp = 57; act = " ++ show ( frames2ScoreMap doublePinfall ) ++ "\n"  
    putStrLn $ "frames2ScoreTypeMap doublePinfall; exp = 300; act = " ++ show ( frames2ScoreTypeMap doublePinfall ) ++ "\n" 
    
    putStrLn $ "frames2ScoreDirect turkeyPinfall = exp 105 " ++ show ( frames2ScoreDirect scoreDefault turkeyPinfall ) ++ "\n" 
    putStrLn $ "frames2ScoreMap turkeyPinfall; exp = 105; act = " ++ show ( frames2ScoreMap turkeyPinfall ) ++ "\n" 
    putStrLn $ "frames2ScoreTypeMap turkeyPinfall; exp = 300; act = " ++ show ( frames2ScoreTypeMap turkeyPinfall ) ++ "\n" 
    
    putStrLn $ "frames2ScoreDirect sparePinfall = exp 20 " ++ show ( frames2ScoreDirect scoreDefault sparePinfall ) ++ "\n" 
    putStrLn $ "frames2ScoreMap sparePinfall; exp = 20; act = " ++ show ( frames2ScoreMap sparePinfall ) ++ "\n" 
    putStrLn $ "frames2ScoreTypeMap sparePinfall; exp = 300; act = " ++ show ( frames2ScoreTypeMap sparePinfall ) ++ "\n" 
    
    putStrLn $ "frames2ScoreDirect game1 = exp 168 " ++ show ( frames2ScoreDirect scoreDefault game1 ) ++ "\n" 
    putStrLn $ "frames2ScoreMap game1; exp = 168; act = " ++ show ( frames2ScoreMap game1 ) ++ "\n" 
    putStrLn $ "frames2ScoreTypeMap game1; exp = 300; act = " ++ show ( frames2ScoreTypeMap game1 ) ++ "\n" 
    
--    Input error checks 
    putStrLn $ "\nInput Checks \n "
    let badFrames = [(-1,-2),(4,-5),(11,-3),(1,11),(10,9),(8,8),(7,7),(6,6),(5,5),(1,9),(2,8),(3,7)]      
    
    putStrLn $ "frames2ScoreDirect badFrames = " ++ show ( frames2ScoreDirect scoreDefault badFrames ) ++ "\n" 
    
    putStrLn $ "inValidFrames = " ++ "\n" ++ intercalate "\n" ( inValidFrames badFrames )  
    
    putStrLn $ "areAllFramesOK = " ++ (fromLeft "Yes" (areAllFramesOK badFrames) ) ++ "\n"  

--    property based testing 
--    note these are invalid, as they use an overly simple scoring algorithm 
--    they are included to demo how this could be done with more work 
    putStrLn $ "\nProperty Tests \n "
    
    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter = " ++ show ( prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter badFrames ) ++ "\n" 
    
    putStrLn $ "frames2ScoreMap = " ++ show ( frames2ScoreMap badFrames ) ++ "\n" 

    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_OK = "
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_OK  
    
    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter = " 
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreDirect_No_filter  

    putStrLn $ "prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_Wrong = " 
    quickCheck prop_frames2ScoreMap_eq_frames2ScoreDirect_filter_Wrong  

    putStrLn $ "frames2ScoreMap perfectGame = "     
    
    mapM_ (putStrLn.show) [(frames2ScoreMap perfectGame)]  
    
--------------------------
--    Not used
--    included for discussion only 
    
-- function equality 
fnEq :: Eq (a -> b) => (a -> b) -> (a -> b) -> Bool 
fnEq f g = f == g 
    
--    function equality property  
--    But error: 
--    No instance for (Eq (Frames -> Bool)) 
prop_frames2ScoreMap_eq_frames2ScoreDirect_fn :: (Frames -> Bool) -> (Frames -> Bool) -> Bool 
prop_frames2ScoreMap_eq_frames2ScoreDirect_fn frames2ScoreMap frames2ScoreDirect
--    = fnEq frames2ScoreMap frames2ScoreDirect 
    = undefined     

    
