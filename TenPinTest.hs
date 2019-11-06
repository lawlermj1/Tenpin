{-# LANGUAGE FlexibleContexts #-} 

--    assume that input is Frames list 
--    include spare, strike 
--    exclude fouls, splits 

import Data.Either 
import Data.List  
import Test.QuickCheck 

import Tenpin 

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
    