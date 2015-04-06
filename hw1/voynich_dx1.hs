import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M 
import System.IO
import Control.Monad
import Control.Monad.Trans.State.Lazy

-- Some type synonyms
type Multimap = M.Map String (M.Map String Int)
type MapTuple = (Char, String, Multimap)

main :: IO ()
main = do
    evalStateT setup (' ',"",filemaps)
    content <- readFile "interlin.txt"
    let fileLines = lines content
    (_,_,maps) <- execList (map processLine fileLines) ('A',"1",filemaps)
    let goodmaps = filter (\(k,v) -> v /= M.empty) (M.toList maps)
    mapM_ exportMap goodmaps
    return ()

-- Initial state setup
setup :: StateT MapTuple IO ()
setup = do
    put (' ',"",filemaps)
    return ()

-- Function to execute a list of States in sequence
execList :: [StateT MapTuple IO ()] -> MapTuple -> IO MapTuple
execList [] st = return st
execList (x:xs) st = do
    result <- execStateT x st
    new <- execList xs result
    return new

-- Function for processing a line of text
processLine :: String -> StateT MapTuple IO ()
processLine line = do
    (l,h,ms) <- get
    let lt = lineType line
    let readC = readComment line
    let (newL, newH) = if readC /= Nothing then fromJust readC else (' ',"")
    case lt of Nothing -> return ()
               Just Comment -> if readC /= Nothing then put (newL,newH,ms) else return ()
               Just Transcription -> do
                    let (info, trans) = case readTranscription line of Just (x,y) -> (x, y)
                                                                       Nothing -> ("","")
                    if (info, trans) /= ("","") then do
                        let ms' = updateMap (l, h) (info, trans) ms
                        put (l, h, ms')
                    else return ()
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
removeBetween _ _ "" = ""
removeBetween c1 c2 s
    | not ((c1 `elem` s) && (c2 `elem` s)) = s
    | otherwise = beginning ++ (removeBetween c1 c2 (tail end))
    where (beginning, rest) = break (== c1) s
          (_, end) = break (== c2) rest

-- this is a helper function used to deal with the
-- instances when we have a word break in between
-- square brackets. It replaces any instance of a period
-- in square brackets with a +, and we will then change
-- this + back to a period later. This allows us to treat
-- anything with brackets as one word even if there are
-- periods (i.e. spaces) within the brackets.
correctBrackets :: String -> String
correctBrackets "" = ""
correctBrackets s
    | not (('[' `elem` s) && (']' `elem` s)) = s
    | otherwise = beginning ++ (fix inBrackets) ++ (correctBrackets end)
    where (beginning, rest) = break (== '[') s
          (inBrackets, end) = break (== ']') rest
          fix t = map (\x -> if (x == '.') then '+' else x) t

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
-- specified language will evaluate to Nothing.
-- If a language but no hand is specified, then we
-- take the hand to be hand 0.
readComment :: String -> Maybe (Char, String)
readComment s
    | "language" `isInfixOf` (map toLower s) = Just (language, hand)
    | otherwise = Nothing
    where language = toUpper $ last $ filter (/= '\r') $ head splitString
          splitString = splitOnChar ',' s
          hand = case splitString of [] -> "0"
                                     [x] -> "0"
                                     [x,y] -> last $ words y

-- function to read a transcription line
-- evaluates to nothing unless the line is of the correct form, i.e.,
-- <info>                         transcription
-- in which case it evaluates to (<info>, transcription)
readTranscription :: String -> Maybe (String, String)
readTranscription line = case (words line) of [] -> Nothing
                                              [x] -> Nothing
                                              (x:xs) -> if (';' `elem` x) then
                                                    Just (x, takeWhile (\x -> (x /= '-') && (x /= '=')) $ concat xs)
                                              else Nothing

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
hands = ["0","1","2","3","4","4?","5","X","Y"]

-- a list of all possible files, i.e., all possible combinations
-- of language/hand/source. Not all will be used.
filenames :: [String]
filenames = [filename (l, h, s) | l <- languages, h <- hands, s <- sources]

-- map from file names to their corresponding (word, count) maps
-- starts empty but will update as we read the file
filemaps :: Multimap
filemaps = M.fromList $ zip filenames (repeat M.empty)
--filemaps = M.insert "test.dx1" (updateMapFromLine M.empty "this.is.a.test.yes.this.is") (M.fromList $ zip filenames (repeat M.empty))

-- Update a map given a word. Either increase the count of that
-- word, or set it to 1 if it is not in the map already.
updateFromWord :: (Ord k, Num a) => M.Map k a -> k -> M.Map k a
updateFromWord m word = do
    let value = M.lookup word m
    case value of Nothing -> M.insert word 1 m
                  Just x -> M.adjust (+1) word m

-- Updates the values in the map m corresponding to the words
-- in line. This expects the second part of the line, i.e.,
-- only the transcription (not the things in angle brackets)
updateMapFromLine :: M.Map String Int -> String -> M.Map String Int
updateMapFromLine m line = foldl updateFromWord m ws'
    where ws' = map (map (\x -> if x == '+' then '.' else x)) ws
          ws = splitOnChar '.' $ removeChars ['!','%'] (removeBetween '{' '}' $ correctBrackets line)

-- Takes (lang, hand) and (<info>, transcription), then
-- updates the appropriate map from the map of maps it is given
updateMap :: (Char, String) -> (String, String) -> M.Map String (M.Map String Int) -> M.Map String (M.Map String Int)
updateMap (l,h) (info, trans) ms = M.insert key (updateMapFromLine m trans) ms
    where m = fromJust $ M.lookup key ms
          key = filename (l,h, extractSource info)

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
exportMap :: (Show a, Ord a) => (String, M.Map String a) -> IO ()
exportMap (path, m) = writeFile ("output/" ++ path) (concat (comments : strings))
    where strings = map (\(k,v) -> k ++ "\t" ++ (show v) ++ "\n") (sortMap m)
          comments = unlines ["# dx1 file made by Noah Halford"
                             ,"# Dictionary file for Voynich manuscript language " ++ l:","
                             ,"# hand " ++ h ++ ", and source " ++ [s]
                             ,"# It is unclear exactly what asterisks (*) in this file mean"
                             ,"# so they were treated as part of the alphabet."
                             ,"# Comments, exclamation points (!), and percent"
                             ,"# symbols (%) were removed."
                             ,"# Varations (of the form [A|B]) were not treated"
                             ,"# specially. That is, anything of the form xxx[xxx|xxx]xxx"
                             ,"# was treated as one word, even if there were periods,"
                             ,"# i.e., spaces, within the brackets."]
          l = last lang
          (lang,rest) = break (== '_') path
          (h, end) = break (== '_') $ tail rest
          s = head $ tail end
