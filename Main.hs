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
    north (r,c) = [(r-1,c)]
    south (r,c) = [(r+1,c)]
    west (r,c) =  [(r, c-1)]
    east (r,c) =  [(r, c+1)]
    neighbors (r,c) = north(r,c) ++ south(r,c) ++ west(r,c) ++ east(r,c)
    ((rLo,cLo),(rHi,cHi)) = bounds f
    validSeep = filter (\p@(r,c) -> r >= rLo && 
                                    r <= rHi && 
                                    c >= cLo && 
                                    c <= cHi && 
                                    f!p == ' ') $ nub $ sort seep
 
-- Percolate a field;  Return the percolated field.
percolate :: Field -> Field
percolate start@(Field f _ _) = 
    let ((_,_),(_,cHi)) = bounds f
        (final, _) = percolateR [(0,c) | c <- [0..cHi]] start
    in final
 
-- Generate a random field.
randomField :: Int -> Int -> Double -> Rand StdGen Field
randomField rows cols threshold = do
    let toChar t = if (t<threshold) then ' ' else '#'
    rnd <- fmap toChar <$> getRandoms 
    let f = listArray ((0,0), (rows-1, cols-1)) rnd

    hrnd <- fmap (<threshold) <$> getRandoms
    let h = listArray ((0,0),(rows-1, cols)) hrnd

    vrnd <- fmap (<threshold) <$> getRandoms 
    let v = listArray ((0,0),(rows, cols-1)) vrnd

    return $ Field f h v
 
-- Assess whether or not percolation reached bottom of field.
leaky :: Field -> Bool
leaky (Field f _ _) = '.' `elem` [f!(rHi,c) | c <- [cLo..cHi]] where
               ((_,cLo),(rHi,cHi)) = bounds f
 
-- Run test once; Return bool indicating success or failure.
oneTest :: Int -> Int -> Double -> Rand StdGen Bool
oneTest rows cols threshold = 
    leaky <$> percolate <$> randomField rows cols threshold
 
-- Run test multple times; Return the number of tests that pass
multiTest :: Int -> Int -> Int -> Double -> Rand StdGen Double
multiTest repeats rows cols threshold = do
    results <- replicateM repeats $ oneTest rows cols threshold
    let leakyCount = length $ filter (==True) results
    return $ fromIntegral leakyCount / fromIntegral repeats
 
alternate :: [a] -> [a] -> [a]
alternate [] _ = []
alternate (a:as) bs = a : alternate bs as
 
showField :: Field -> IO ()
showField (Field a h v) =  do
    let ((rLo,cLo),(rHi,cHi)) = bounds a
        fLines =  map show [ [ a!(r,c) | c <- [cLo..cHi]] | r <- [rLo..rHi]]
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
  mapM_ print [format ("p=" % int % "/" % int % " -> " % fixed 4) n d r | (n,r) <- results]
 
