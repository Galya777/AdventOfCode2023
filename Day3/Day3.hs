import Data.Char (isDigit)
import Data.List (tails)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Parse a single character as a digit, returning 0 for non-digits.
parseDigit :: Char -> Int
parseDigit c
  | isDigit c = read [c]
  | otherwise = 0

-- | Parse a line into a list of integers.
parseLine :: T.Text -> [Int]
parseLine = map parseDigit . T.unpack

-- | Check if a character is a symbol.
isSymbol :: Char -> Bool
isSymbol c = c `elem` "*+#$."

-- | Check if a character is a valid part number.
isValidPart :: Char -> Bool
isValidPart c = isDigit c || isSymbol c

-- | Sum of valid part numbers adjacent to a symbol in a list of lines.
sumAdjacentParts :: [T.Text] -> Int
sumAdjacentParts = sum . map (sum . map sum . filter (not . null) . map (map parseDigit) . tails . T.unpack) . filter (not . null) . map (filter isValidPart . T.unpack)

main :: IO ()
main = do
  contents <- TIO.readFile "/home/galya777/IdeaProjects/AdventOfCode2023/Day3/input.txt"
  let linesOfFile = T.lines contents
  print $ sumAdjacentParts linesOfFile
