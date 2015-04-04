{-
 - A list of important functions:
 - extractSource :: String -> Char
 --- Finds source from typical format angle brackets
 - lineType :: String -> Maybe Line
 --- determines if a line is a comment or a transcription
 --- (or neither)
 - readComment :: String -> Maybe (Char, String)
 --- Returns Just (language, hand) if the comment contains
 --- that information and Nothing otherwise
 - readTranscription :: String -> Maybe (String, String)
 --- splits a transcription line into (info, words)
 - filename :: (Char, String, Char) -> String
 --- create dx1 file name from (lang, hand, source)
 - updateMap :: (Ord k, Num a) => M.Map k a -> k -> M.Map k a
 --- given a word, increases the count of that word in the map by 1
 - updateMapFromLine :: M.Map String Int -> String -> M.Map String Int
 --- given a sequence of words, updates the map with their appropriate
 --- counts
 - exportMap :: (Show a, Ord a) => String -> M.Map String a -> IO ()
 --- writes map to file given by the string
 -}

import Data.Char
import Data.List
import qualified Data.Map.Strict as M 
import System.IO

main :: IO ()
main = do
    content <- readFile "interlin.txt"
    let fileLines = lines content
    mapM_ print fileLines
    return ()

-- function to remove characters from a string
-- used for removing meta characters
removeChars :: [Char] -> String -> String
removeChars cs = filter (not . (flip elem cs))

-- function to remove characters between two special
-- characters. Used to remove characters between
-- curly braces, which are treated as comments
-- We assume that if c1 occurs in a line, then
-- c2 occurs later in that line. We also assume
-- there are no nested comments, i.e., anything
-- of the form c1 xxxx c1 xxxx c2 xxxx c2
removeBetween :: Char -> Char -> String -> String
removeBetween _ _ [] = []
removeBetween c1 c2 s
    | not ((c1 `elem` s) && (c2 `elem` s)) = s
    | otherwise = beginning ++ (removeBetween c1 c2 (tail end))
    where (beginning, rest) = break (== c1) s
          (_, end) = break (== c2) rest

-- similar to words from Prelude, but splits on a
-- a specified character rather than on whitespace
-- This is just a modified definition of words as given at
-- https://www.haskell.org/onlinereport/standard-prelude.html
splitOnChar :: Char -> String -> [String]
splitOnChar c s = case dropWhile (== c) s of
                    "" -> []
                    s' -> w : splitOnChar c s''
                        where (w, s'') = break (== c) s'

-- function to get the source from within angle brackets
-- e.g., extracts 'C' from <f1r.1;C>
extractSource :: String -> Char
extractSource = last . init

-- data type for the different kinds of lines in the input file
data Line = Comment | Transcription deriving Show

-- function to determine line type
lineType :: String -> Maybe Line
lineType line
    | head line == '#' = Just Comment
    | head line == '<' = Just Transcription
    | otherwise = Nothing

-- function to read comments and extract language and hand
-- we use String for the hand type because possibilities
-- include numbers, X/Y, "4?", and none. The case of none
-- will be given by an empty string, i.e., ""
-- A comment of the form "# Currier's language A, hand 1"
-- will evalueate to Just ('A',"1"). Comments with no
-- specified language will evaluate to Nothing
readComment :: String -> Maybe (Char, String)
readComment s
    | "language" `isInfixOf` (map toLower s) = Just (language, hand)
    | otherwise = Nothing
    where language = toUpper $ last $ head splitString
          splitString = splitOnChar ',' s
          hand = case splitString of [] -> ""
                                     [x] -> ""
                                     [x,y] -> last $ words y

-- function to read a transcription line
-- evaluates to nothing unless the line is of the correct form, i.e.,
-- <info>                         transcription
-- in which case it evaluates to (<info>, transcription)
readTranscription :: String -> Maybe (String, String)
readTranscription line = case (words line) of [] -> Nothing
                                              [x] -> Nothing
                                              (x:xs) -> Just (x, takeWhile (/= '-') $ concat xs)

-- Takes (lang, hand, source), converts to name of
-- output dx1 file
filename :: (Char, String, Char) -> String
filename (l,h,s) = "voynich" ++ l:"_" ++ h ++ "_" ++ s:".dx1"

languages, sources :: [Char]

-- list of all languages
languages = ['A','B']

-- list of all sources
sources = ['C','F','T','L','R','K','J']

-- list of all hands
hands :: [String]
hands = ["","1","2","3","4","4?","5","X","Y"]

-- a list of all possible files, i.e., all possible combinations
-- of language/hand/source. Not all will be used.
filenames :: [String]
filenames = [filename (l, h, s) | l <- languages, h <- hands, s <- sources]

-- map from file names to their corresponding (word, count) maps
-- starts empty but will update as we read the file
filemaps :: M.Map String (M.Map String Int)
filemaps = M.fromList $ zip filenames (repeat M.empty)

-- Update a map given a word. Either increase the count of that
-- word, or set it to 1 if it is not in the map already.
updateMap :: (Ord k, Num a) => M.Map k a -> k -> M.Map k a
updateMap m word = do
    let value = M.lookup word m
    case value of Nothing -> M.insert word 1 m
                  Just x -> M.adjust (+1) word m

-- Updates the values in the map m corresponding to the words
-- in line. This expects the second part of the line, i.e.,
-- only the transcription (not the things in angle brackets)
updateMapFromLine :: M.Map String Int -> String -> M.Map String Int
updateMapFromLine m line = foldl updateMap m ws
    where ws = splitOnChar '.' $ removeChars ['!','%'] (removeBetween '{' '}' line)

-- helper function for sortMap: sorts first by b (corresponding
-- to counts), then by a. Note that we compare in the opposite
-- orders so that we eventually sort (in sortMap) by descending
-- counts but alphabetically within each count
comparePair :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
comparePair (x1, y1) (x2, y2)
    | y1 /= y2 = compare y2 y1
    | otherwise = compare x1 x2

-- Given a map, sort by descending value and then increasing key
-- For our purposes, we'll sort by descending count and, within
-- each count, alphabetically from A to Z
sortMap :: (Ord k, Ord a) => M.Map k a -> [(k,a)]
sortMap = sortBy comparePair . M.toList

-- write map m to file at path, sorted first by count
-- and then alphabetically
exportMap :: (Show a, Ord a) => String -> M.Map String a -> IO ()
exportMap path m = writeFile path (concat strings)
    where strings = map (\(k,v) -> k ++ "\t" ++ (show v) ++ "\n") (sortMap m)

{- TODO:
 - update (l, h) state
 - IO
 - include notes on choices
-}
