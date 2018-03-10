
# Sample Ten Pin using Haskell for Rokt 

There are 3 parts for code review. 
1. Scoring
2. Input checks
3. Property Tests

Further comments are in the program. 

## Scoring 
This was a little tricky as scoring needs to look ahead 2 frames before the score can be finalised. 
Normally, a result just requires the current value, which can be easily recursed with a map or fold. 
As a further gotcha, the final value had to be truncated from a running total score. 
There were 5 examples from the wiki and thoughtco site. 
The scoring was correct for these 5. 
Naturally, more complete testing is needed. 
I added a map scoring function, so now there are 2 valid scoring functions. 

## Input Tests
These are some simple checks such as non-negativity, max values, etc. 

## Property Tests 
The 2 scoring functions can now be used for valid property tests. 
Then quickcheck was called to run a 100 random value tests automatically for each of the 3 property tests. 
Results were as expected. 
