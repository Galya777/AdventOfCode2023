import System.IO

readMap :: IO [[Char]]
readMap = do
    handle <- openFile "input.txt" ReadMode
    column <- fmap read (hGetLine handle)
    row <- fmap read (hGetLine handle)
    content <- sequence [hGetLine handle | _ <- [1..row]]
    hClose handle
    return (map (\line -> take column line) content)

main :: IO ()
main = do
    dataMap <- readMap
    let sumDigits = sum [read [c] | row <- dataMap, c <- reverse row, isDigit c]
    print sumDigits
  where
    isDigit c = c >= '0' && c <= '9'
