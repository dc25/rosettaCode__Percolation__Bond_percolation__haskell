{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad
import           Control.Monad.Random
import           Data.Array.Unboxed
import           Data.List
import           Formatting
 
data Field = Field { f :: UArray (Int, Int) Char  
                   , hWall :: UArray (Int, Int) Bool  
                   , vWall :: UArray (Int, Int) Bool  
                   }

 
-- Start percolating some seepage through a field.
-- Recurse to continue percolation with spreading seepage.
percolateR :: [(Int, Int)] -> Field -> (Field, [(Int,Int)])
percolateR [] (Field f h v) = (Field f h v, [])
percolateR seep (Field f h v) = percolateR   
                       (concatMap neighbors validSeep)
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
    let ((_,_),(xHi,_)) = bounds f
        (final, _) = percolateR [(x,0) | x <- [0..xHi]] start
    in final
 
-- Generate a random field.
initField :: Int -> Int -> Double -> Rand StdGen Field
initField width height threshold = do
    let toChar t = if t<threshold then ' ' else '#'
    rnd <- fmap toChar <$> getRandoms 
    let f = listArray ((0,0), (width-1, height-1)) rnd

    hrnd <- fmap (<threshold) <$> getRandoms
    let h0 = listArray ((0,0),(width, height-1)) hrnd
        h1 = h0 // [((0,y), True) | y <- [0..height-1]]
        h2 = h1 // [((width,y), True) | y <- [0..height-1]]

    vrnd <- fmap (<threshold) <$> getRandoms 
    let v0 = listArray ((0,0),(width-1, height)) vrnd
    let v1 = v0 // [((x,0), True) | x <- [0..width-1]]

    return $ Field f h2 v1
 
-- Assess whether or not percolation reached bottom of field.
leaky :: Field -> Bool
leaky (Field f _ _) = '.' `elem` [f!(x,yHi) | x <- [xLo..xHi]] where
               ((xLo,yLo),(xHi,yHi)) = bounds f
 
-- Run test once; Return bool indicating success or failure.
oneTest :: Int -> Int -> Double -> Rand StdGen Bool
oneTest width height threshold = 
    leaky . percolate <$> initField width height threshold
 
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
        fLines =  [ [ a!(x,y) | x <- [xLo..xHi]] | y <- [yLo..yHi]]
        hLines =  [ [ if h!(x,y) then '|' else ' ' | x <- [xLo..xHi+1]] | y <- [yLo..yHi]]
        vLines =  [ [ if v!(x,y) then '-' else ' ' | x <- [xLo..xHi]] | y <- [yLo..yHi+1]]
        lattice =  [ [ '+' | x <- [xLo..xHi+1]] | y <- [yLo..yHi+1]]

        hDrawn = zipWith alternate hLines fLines
        vDrawn = zipWith alternate lattice vLines
        aDrawn = alternate vDrawn hDrawn
    mapM_ putStrLn aDrawn


main :: IO ()
main = do
  g <- getStdGen
  let (startField, g2) = runRand (initField 15 15 0.6) g
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
 
