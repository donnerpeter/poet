{-#LANGUAGE TupleSections,BangPatterns#-}
module Poet where

import Data.Ord
import Data.Word
import Data.Bits
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Russian

solve = putStrLn (renderLines lines ++ "\nAlliterations: " ++ show (alliterationCount lines)) where
  lines = tryFixPhonetics $ fillShapes solvedShapes

--solvedShapes = solveShapes
solvedShapes = [[WordShape {wsTotal = 2, wsAccent = 1}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 3, wsAccent = 3}],[WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 3, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 2}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 1}],[WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 2, wsAccent = 1}],[WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 1}],[WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 2, wsAccent = 1}],[WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 4, wsAccent = 3}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 4, wsAccent = 3}],[WordShape {wsTotal = 1, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 3, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 1}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 3, wsAccent = 3}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 4, wsAccent = 4},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 3, wsAccent = 3}],[WordShape {wsTotal = 4, wsAccent = 3},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 3, wsAccent = 3}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 2, wsAccent = 1}],[WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 3, wsAccent = 2},WordShape {wsTotal = 2, wsAccent = 1}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}],[WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 2, wsAccent = 1},WordShape {wsTotal = 1, wsAccent = 0},WordShape {wsTotal = 2, wsAccent = 2},WordShape {wsTotal = 1, wsAccent = 0}]]
----------------------

hasAlliteration :: [String] -> Bool
hasAlliteration allWords = any sameStart $ zip3 elemWords (drop 1 elemWords) (drop 2 elemWords) where
  elemWords = filter elemWord allWords
  sameStart ((c:_), w2, w3) = [c] `isPrefixOf` w2 && [c] `isPrefixOf` w3

alliterationCount :: [[String]] -> Int
alliterationCount lines = length $ filter hasAlliteration lines

similarity = \w1 w2 -> similarities M.! (order w1 w2) where
  order w1 w2 = (min w1 w2, max w1 w2)
  similarities = M.fromList [((order w1 w2), calcSimilarity w1 w2) | w1 <- elements, w2 <- elements, w1 /= w2] where
    lcs [] _ = 0
    lcs _ [] = 0
    lcs (x:xs) (y:ys) = if x == y then 1 + lcs xs ys else max (lcs (x:xs) ys) (lcs xs (y:ys))
    toInt b = if b then 1 else 0
    rhymeClassOf w = findIndex (w `elem`) postProcessedRhymes
    calcSimilarity w1 w2 = 1 + foldl' (\a i -> a * 10 + i) 0 [
     toInt $ head w1 == head w2,
     toInt $ rhymeClassOf w1 == rhymeClassOf w2,
     lcs w1 w2, 
     toInt $ shape w1 == shape w2,
     S.size $ S.intersection (S.fromList w1) (S.fromList w2)]

closestElements = M.fromList [(e, reverse $ sortBy (comparing $ similarity e) $ delete e elements) | e <- elements]

-----------------------

lineSyllables = 16

remainingWords = filter (\w -> not $ S.member w templateWords) elements
templateWords = S.fromList $ concatMap (\p -> fst p ++ snd p) template
remainingShapes = map shape remainingWords

-----------------------

data WordShape = WordShape { wsTotal :: Int, wsAccent :: Int } deriving (Show, Eq, Ord)

shape word = result where
 sc = syllableCount word
 accent = M.findWithDefault (-1) word elementsWithAccents
 result =
   if sc <= 0 || accent < 0 || accent > sc then error $ "Incorrect syllables/accent in " ++ word
   else WordShape sc accent
fillerShape = WordShape 1 0

allShapes = S.elems $ S.fromList $ map shape elements

------------------------------

allMarkupContinuations :: [[[WordShape]]]
allMarkupContinuations = cycle (map markupFrom [0..3]) where
  accentMatches pos shape = (if wsTotal shape > 2 then isStrongAccentPosition else isAccentPosition) (pos + (wsAccent shape) - 1)
  markupFrom :: Int -> [[WordShape]]
  markupFrom pos = filter (accentMatches pos) allShapes >>= \shape ->
    if isStrongAccentPosition (pos + wsTotal shape) then [[shape]]
    else [[shape], [shape, fillerShape]]    

markLine start end = filter (bounded maxVector . toVector) $ filter fillersSoundNice $ inner 0 start where
  fillersSoundNice middleShapes = elemIndex fillerShape (reverse middleShapes) /= Just 1
  inner wordCount pos =
    if pos > end || wordCount > 7 then []
    else if pos == end then [[]]
    else (allMarkupContinuations !! pos) >>= \step ->
      map (step ++) $ inner (wordCount + 1) (pos + sum (map wsTotal step))

templateMarkups :: [[[WordShape]]]
templateMarkups = zipWith limitWordCount (map (\lineTemplate -> (M.!) cache (query lineTemplate)) template) [0..] where
  limitWordCount markups lineIndex = filter (\shapes -> if lineIndex >= 22 then elemCount shapes > 4 else elemCount shapes < 6) markups where
    elemCount shapes = length $ filter (/= fillerShape) shapes
  queries = map query template
  query (prefix, suffix) = (sum $ map syllableCount prefix, lineSyllables - sum (map syllableCount suffix))
  cache = M.fromList $ map (\(start, end) -> ((start, end), markLine start end)) queries

isAccentPosition pos = pos `mod` 2 == 1
isStrongAccentPosition pos = pos `mod` 4 == 1

---------------------------

type ShapeVector = Int

placeBits = 6
vectorLength = 8
placeMax = shift 1 placeBits - 1

shape2VectorPlace :: M.Map WordShape Int
shape2VectorPlace =
  if length allShapes > vectorLength then error $ "more shapes " ++ show (length allShapes) ++ " than fits"
  else M.fromList $ zip allShapes [0..]

toVector :: [WordShape] -> ShapeVector
toVector shapes = listToVector $ map (\i -> M.findWithDefault 0 i index2Count) [0..vectorLength - 1] where
  index2Count = M.fromListWith (+) $ map (\s -> (shape2VectorPlace M.! s, 1)) $ filter (/= fillerShape) shapes

listToVector :: [Int] -> ShapeVector
listToVector list =
  if not (and $ map (\i -> fromIntegral i <= placeMax) list) then error $ "placeMax < " ++ show list
  else sum $ zipWith (\val index -> shift (fromIntegral val) (index * placeBits)) list [0..vectorLength - 1]
  
templateVectors = map (\lineMarkups -> removeDuplicates id $ map toVector lineMarkups) templateMarkups

removeDuplicates by xs = inner IS.empty xs where
  inner _ [] = []
  inner visited (x:xs) = let eq = by x in
    if eq `IS.member` visited then inner visited xs else x:(inner (IS.insert eq visited) xs)

-------------

type SumLine = [(ShapeVector, ShapeVector)]

solvePart :: [[ShapeVector]] -> ShapeVector -> Maybe [ShapeVector] 
solvePart lineVectors targetVector = getPath (length table - 1) targetVector [] where
  table = map addSumLine $ zip ([(0,0)]:table) lineVectors
  addSumLine :: (SumLine, [ShapeVector]) -> SumLine
  addSumLine (prev, vs) = traceShow ("sumLine", length prev) $ removeDuplicates fst $ filter (bounded targetVector . fst) [(fst prevSum + v, v) | prevSum <- prev, v <- vs]
  lastRowBytes = map (vecBytes . fst) $ last table
  getPath index targetSum result =
    if index < 0 then Just result
    else case lookup targetSum (table !! index) of
      Just vec -> getPath (index - 1) (targetSum - vec) (vec : result)
      Nothing -> error $
        "cannot find " ++ show (vecBytes targetSum) ++
        " (" ++ (show $ sum $ vecBytes targetSum) ++ " words) " ++
        "in line " ++ show index ++
        ", max in each dimension = " ++ show (foldl1' (zipWith max) lastRowBytes) ++
        ", closest = " ++ show (take 10 $ sortBy (comparing $ distance $ vecBytes targetVector) lastRowBytes) ++
        ", max word count = " ++ show (maximum $ map sum lastRowBytes)

template1 = take 12 templateVectors
template2 = drop 12 templateVectors
target1 = toVector $ take (length remainingShapes `div` 2) remainingShapes
target2 = maxVector - target1
maxVector = toVector remainingShapes

bounded !targetVector !vec = all (\i -> vecByte vec i <= vecByte targetVector i) [0..vectorLength - 1]

vecByte !vec !i = shift vec (-i * placeBits) .&. placeMax
vecBytes vec = map (vecByte vec) [0..vectorLength - 1]

vecMax xs ys = zipWith max xs ys

distance :: [Int] -> [Int] -> Int
distance xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

solveShapes = zipWith markupByVector [0..] $ fromJust (solvePart template1 target1) ++ fromJust (solvePart template2 target2) where
  markupByVector :: Int -> ShapeVector -> [WordShape]
  markupByVector lineIndex vec = head $ preferredMarkups ++ suitableMarkups where
    suitableMarkups = filter (\s -> toVector s == vec) $ templateMarkups !! lineIndex
    preferredMarkups = filter ([fillerShape] `isSuffixOf`) suitableMarkups

---------------------

fillShapes :: [[WordShape]] -> [[String]]
fillShapes shapeLines = zipWith (\middle (prefix, suffix) -> prefix ++ middle ++ suffix) shapeWords template where
  shapeWords = map (\i -> M.findWithDefault ["???"] i mapping) lineIndices
  lineIndices = [0 .. length shapeLines - 1] 

  fillLine :: S.Set String -> Int -> [String]
  fillLine _availableWords line = maximumBy (comparing $ rating line) $ reverse allOptions where
    allOptions = map tryLetter allLetters
    allLetters = S.elems $ S.map head $ S.filter (\w -> shape w `elem` shapes) _availableWords
    shapes = shapeLines !! line
    bestMatching c words = fromMaybe (head words) $ find ([c] `isPrefixOf`) words
    foreachFold list var body = foldl' body var list
    tryLetter c = snd $ foreachFold shapes (_availableWords, []) $ \(availableWords, result) eachShape ->
      let word = if eachShape == fillerShape then conjunct else bestMatching c $ filter (\w -> shape w == eachShape) $ S.elems availableWords
      in (S.delete word availableWords, result ++ [word])

  rating line words = homogeneity (words ++ snd (template !! line))

  mapping :: M.Map Int [String]
  mapping = fillLines remainingSet (map (\i -> (i, fillLine remainingSet i)) lineIndices) M.empty where
    remainingSet = S.fromList remainingWords
    fillLines availableWords options result = answer where
      (bestLine, bestWords) = maximumBy (comparing $ \(line, words) -> rating line words) $ reverse options
      usedWords = S.fromList bestWords
      nextAvailable = S.difference availableWords usedWords
      nextOptions = map (\(i, words) -> (i, if any (`S.member` usedWords) words then fillLine nextAvailable i else words)) $ 
        filter (\p -> fst p /= bestLine) options
      answer = if null options then result 
               else fillLines nextAvailable nextOptions (M.insert bestLine bestWords result)

homogeneity words = if sameLetterPairs == length elemWords then 100 else sameLetterPairs where
  sameLetterPairs = length $ filter (\(w1, w2) -> head w1 == head w2) $ zip elemWords (drop 1 elemWords)
  elemWords = filter elemWord words

elemWord w = w `M.member` elementsWithAccents
  
------------

type Location = (Int, Int)

wordsWithLocations :: [[a]] -> [(a, Location)]
wordsWithLocations lines = concat $ zipWith processLine lines [0..] where
  processLine lineWords line = zipWith (\word col -> (word, (line, col))) lineWords [0..]

findPhoneticIssues :: [[String]] -> [Location]
findPhoneticIssues lines = concat $ zipWith3 (\prev (word, loc) next -> if hasIssue prev word next then [loc] else []) 
                                     ([""] ++ words) (wordsWithLocations lines) (drop 1 words ++ [""]) where
  words = concat lines
  hasIssue prev word next = (issueBetween prev word || issueBetween word next) && word `elem` remainingWords where
    issueBetween w1 w2 = sameLetter w1 w2 || w2 == "ртуть" && ("ть" `isSuffixOf` w1 || "т" `isSuffixOf` w1) 
    sameLetter w1 w2 = not (null w1) && [last w1] `isPrefixOf` w2

tryFixPhonetics :: [[String]] -> [[String]]
tryFixPhonetics lines = case findPhoneticIssues lines of
  [] -> lines
  some:_ -> case fixPhoneticIssue lines some of
    Nothing -> lines
    Just better -> tryFixPhonetics better

fixPhoneticIssue :: [[String]] -> Location -> Maybe [[String]]
fixPhoneticIssue lines loc = find isBetter $ sortBy (comparing priority) allSwaps where
  isBetter ls = length (findPhoneticIssues ls) < issueCount where
    issueCount = length (findPhoneticIssues lines)
  priority candidateLines = (-(alliterationCount lines), -(homogeneity $ concat candidateLines))
  allSwaps = map swapWords $ reverse alternativeLocations
  word = lines !! fst loc !! snd loc
  alternativeLocations = filter (\(w, _) -> shape w == shape word && w /= word && not (S.member w templateWords)) $ wordsWithLocations lines
  swapWords (anotherWord, loc2) = putWord loc anotherWord $ putWord loc2 word lines where 
    putWord (line, col) w lines = replace line (replace col w $ lines !! line) lines 
    replace index val list = take index list ++ (val : drop (index + 1) list)

--------------------

renderLines lines = concat $ zipWith3 (\p w s -> p ++ w ++ s) prefixes flatWords (suffixes ++ cycle [""]) where
  flatWords = concat lines
  prefixes = concat $ zipWith (\words line -> zipWith (\_ col -> if (line, col) `S.member` issues then "!!!" else "") words [0..]) lines [0..] where
    issues = S.fromList $ findPhoneticIssues lines
  lineFeeds = concatMap (\line -> replicate (length line - 1) "" ++ ["\n"]) lines
  commas = zipWith (\feed tail -> if shouldPutComma feed tail then "," else "") lineFeeds $ tails flatWords
  shouldPutComma feed suffix = case suffix of
    w1:w2:_ | M.member w1 elementsWithAccents && M.member w2 elementsWithAccents -> True
    w1:c:_ | c == conjunct -> False
    c:_ | c == conjunct -> False
    ".":_ -> False
    _ -> feed == "\n"
  suffixes = zipWith3 (\feed comma nextWord -> comma ++ (if nextWord == "." || nextWord == "," || feed == "\n" then feed else " ")) lineFeeds commas (drop 1 flatWords)

