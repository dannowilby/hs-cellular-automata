module Main where

main :: IO ()
main = do
  loop 50 x
  where x = genRow 100

type Row = [Bool]
type Rule = Bool -> Bool -> Bool -> Bool

loop :: Int -> Row -> IO ()
loop 0 _ = return ()
loop n row = printRow row >> loop (n - 1) (applyRule rule18 row)

-- Repeats the characters on the edge when calculating the edge
applyRule :: Rule -> Row -> Row
applyRule rule row = applyRuleStart row : applyRuleHelper row
  where
    applyRuleStart (x : (y : _)) = rule x x y
    applyRuleHelper (x : y : []) = rule x y y : []
    applyRuleHelper (x : y : z : xs) = rule x y z : applyRuleHelper (y:z:xs)

-- Generates a row with the midpoint set to true and the rest false
genRow :: Int -> Row
genRow size = replicate (size `div` 2) False ++ [True] ++ replicate (size `div` 2) False

printRow :: Row -> IO ()
printRow [] = do putStr "\n"
printRow (x:xs) = do putStr (cell x) >> printRow xs 
  where
    cell y = if y then "#" else "-"

rule75 :: Bool -> Bool -> Bool -> Bool
rule75 x y z = case (x, y, z) of 
  (True, True, True) -> False
  (True, True, False) -> True
  (True, False, True) -> False
  (True, False, False) -> False
  (False, True, True) -> True
  (False, True, False) -> False
  (False, False, True) -> True
  (False, False, False) -> True

rule18 :: Bool -> Bool -> Bool -> Bool
rule18 x y z = case (x, y, z) of 
  (True, True, True) -> False
  (True, True, False) -> False
  (True, False, True) -> False
  (True, False, False) -> True
  (False, True, True) -> False
  (False, True, False) -> False
  (False, False, True) -> True
  (False, False, False) -> False
