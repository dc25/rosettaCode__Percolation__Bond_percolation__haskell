{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.Random
import           Data.Array.Unboxed
import           Data.List
import           Formatting
 
data Field = Field { f :: (UArray (Int, Int) Char)  
                   , hWall :: (UArray (Int, Int) Bool)  
                   , vWall :: (UArray (Int, Int) Bool)  
                   }

 
-- Start percolating some seepage through a field.
-- Recurse to continue percolation with spreading seepage.
percolateR :: [(Int, Int)] -> Field -> (Field, [(Int,Int)])
percolateR [] (Field f h v) = (Field f h v, [])
percolateR seep (Field f h v) = percolateR   
                       (concat $ fmap neighbors validSeep) 
                       (Field (f // map (\p -> (p,'.')) validSeep) h v) where
    north (x,y) = [(x,y-1)]
    south (x,y) = [(x,y+1)]
    west (x,y) =  [(x-1,y)]
    east (x,y) =  [(x+1,y)]
    neighbors (x,y) = north(x,y) ++ south(x,y) ++ west(x,y) ++ east(x,y)
    ((xLo,yLo),(xHi,yHi)) = bounds f
    validSeep = filter (\p@(x,y) -> x >= xLo && 
                                    x <= xHi && 
                                    y >= yLo && 
                                    y <= yHi && 
                                    f!p == ' ') $ nub $ sort seep
 
-- Percolate a field;  Return the percolated field.
percolate :: Field -> Field
percolate start@(Field f _ _) = 
    let ((_,_),(_,yHi)) = bounds f
        (final, _) = percolateR [(0,y) | y <- [0..yHi]] start
    in final
 
-- Generate a random field.
randomField :: Int -> Int -> Double -> Rand StdGen Field
randomField width height threshold = do
    let toChar t = if (t<threshold) then ' ' else '#'
    rnd <- fmap toChar <$> getRandoms 
    let f = listArray ((0,0), (width-1, height-1)) rnd

    hrnd <- fmap (<threshold) <$> getRandoms
    let h = listArray ((0,0),(width-1, height)) hrnd

    vrnd <- fmap (<threshold) <$> getRandoms 
    let v = listArray ((0,0),(width, height-1)) vrnd

    return $ Field f h v
 
-- Assess whether or not percolation reached bottom of field.
leaky :: Field -> Bool
leaky (Field f _ _) = '.' `elem` [f!(xHi,y) | y <- [yLo..yHi]] where
               ((_,yLo),(xHi,yHi)) = bounds f
 
-- Run test once; Return bool indicating success or failure.
oneTest :: Int -> Int -> Double -> Rand StdGen Bool
oneTest width height threshold = 
    leaky <$> percolate <$> randomField width height threshold
 
-- Run test multple times; Return the number of tests that pass
multiTest :: Int -> Int -> Int -> Double -> Rand StdGen Double
multiTest repeats width height threshold = do
    results <- replicateM repeats $ oneTest width height threshold
    let leakyCount = length $ filter (==True) results
    return $ fromIntegral leakyCount / fromIntegral repeats
 
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate (a:as) bs = a : alternate bs as
 
showField :: Field -> IO ()
showField (Field a h v) =  do
    let ((xLo,yLo),(xHi,yHi)) = bounds a
        fLines =  map show [ [ a!(x,y) | y <- [yLo..yHi]] | x <- [xLo..xHi]]
    mapM_ putStrLn fLines


main :: IO ()
main = do
  g <- getStdGen
  let (startField, g2) = runRand (randomField 15 15 0.6) g
  putStrLn "Unpercolated field with 0.6 threshold."
  putStrLn ""
  showField startField
 
  putStrLn ""
  putStrLn "Same field after percolation."
  putStrLn ""
  showField $ percolate startField
 
  putStrLn ""
  putStrLn "Results of running percolation test 10000 times with thresholds ranging from 0.0 to 1.0 ."
  let d = 10
  let ns = [0..10]
  let tests = sequence [multiTest 10000 15 15 v 
                           | n <- ns,
                             let v = fromIntegral n / fromIntegral d ]
  let results = zip ns (evalRand tests g2)
  mapM_ print [format ("p=" % int % "/" % int % " -> " % fixed 4) n d x | (n,x) <- results]
 
