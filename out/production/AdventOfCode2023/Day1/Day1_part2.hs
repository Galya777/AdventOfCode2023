import Data.Char (isDigit, digitToInt)

main :: IO ()
main = do
    calibrationDocument <- readCalibrationDocumentFromFile "/home/galya777/IdeaProjects/AdventOfCode2023/Day1_input.txt"
    print (sumCalibrationValues calibrationDocument)

readCalibrationDocumentFromFile :: FilePath -> IO [String]
readCalibrationDocumentFromFile filename = do
    content <- readFile filename
    return (lines content)

sumCalibrationValues :: [String] -> Int
sumCalibrationValues calibrationDocument = sum $ map parseLine calibrationDocument
  where
    parseLine :: String -> Int
    parseLine line =
      let digits = filter isDigit line
      in if not (null digits)
            then digitToInt (head digits) * 10 + digitToInt (last digits)
            else 0
