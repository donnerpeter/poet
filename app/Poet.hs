{-#LANGUAGE TupleSections#-}
module Poet where

import Data.Ord
import Data.Word
import Data.Bits
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Set as S
import qualified Data.Map.Strict as M

solve = putStrLn (renderLines lines ++ "\nAlliterations: " ++ show allits) where
  lines = tryFixPhonetics $ fillShapes solveShapes
  allits = length $ filter hasAlliteration lines

hasAlliteration :: [String] -> Bool
hasAlliteration allWords = any sameStart $ zip3 elemWords (drop 1 elemWords) (drop 2 elemWords) where
  elemWords = filter elemWord allWords
  sameStart ((c:_), w2, w3) = [c] `isPrefixOf` w2 && [c] `isPrefixOf` w3

-----------------------

lineSyllables = 16

syllableCount s = length (vowels s)
isVowel c = c `elem` "аЕеёиоуыэюя"
vowels s = filter isVowel s 

elements = M.keys elementsWithAccents

guessAccent s = let sc = syllableCount s in
  if sc == 1 then Just 1
  else if "ий" `isSuffixOf` s then Just (sc - 1)
  else Nothing
  
guessAccents = mapM f elements where
  f s = case guessAccent s of
    Just i -> putStrLn $ "\"" ++ s ++ "\", " ++ (show i)
    Nothing -> putStrLn $ "\"" ++ s ++ "\", "
    
elementsWithAccents :: M.Map String Int
elementsWithAccents = M.fromList [
  ("водород", 3), ("гелий", 1),
  ("литий", 1), ("бериллий", 2), ("бор", 1), ("углерод", 3), ("азот", 2), ("кислород", 3), ("фтор", 1), ("неон", 2),
  ("натрий", 1), ("магний", 1), ("алюминий", 3), ("кремний", 1), ("фосфор", 1), ("сера", 1), ("хлор", 1), ("аргон", 2),
  ("калий", 1), ("кальций", 1), ("скандий", 1), ("титан", 2), ("ванадий", 2), ("хром", 1), ("марганец", 1), ("железо", 2), 
  ("кобальт", 1), ("никель", 1), ("медь", 1), ("цинк", 1), ("галлий", 1), ("германий", 2), ("мышьяк", 2), ("селен", 2), ("бром", 1), ("криптон", 2),
  ("рубидий", 2), ("стронций", 1), ("иттрий", 1), ("цирконий", 2), ("ниобий", 2), ("молибден", 3), ("технеций", 2),
  ("рутений", 2), ("родий", 1), ("палладий", 2), ("серебро", 3), ("кадмий", 1), ("индий", 1), ("олово", 1), 
  ("сурьма", 2), ("теллур", 2), ("йод", 1), ("ксенон", 2), 
  ("цезий", 1), ("барий", 1), ("лантан", 2), ("церий", 1), ("празеодим", 4), ("неодим", 3), ("прометий", 2), 
  ("самарий", 2), ("европий", 2), ("гадолиний", 3), ("тербий", 1), ("диспрозий", 2), ("гольмий", 1), ("эрбий", 1),
  ("тулий", 1), ("иттербий", 2), ("лютеций", 2), ("гафний", 1), ("тантал", 2), ("вольфрам", 2), ("рений", 1),
  ("осмий", 1), ("иридий", 2), ("платина", 1), ("золото", 1), ("ртуть", 1), ("таллий", 1), ("свинец", 2), ("висмут", 1),
  ("полоний", 2), ("астат", 2), ("радон", 2), 
  ("франций", 1), ("радий", 1), ("актиний", 2), ("торий", 1), ("протактиний", 3), ("уран", 2), ("нептуний", 2),
  ("плутоний", 2), ("америций", 3), ("кюрий", 1), ("берклий", 1), ("калифорний", 3), ("эйнштейний", 2), ("фермий", 1),
  ("менделевий", 3), ("нобелий", 2), ("лоуренсий", 3), ("резерфордий", 3), ("дубний", 1), ("сиборгий", 2), ("борий", 1),
  ("хассий", 1), ("мейтнерий", 2), ("дармштадтий", 2), ("рентгений", 2), ("коперниций", 3), ("нихоний", 2),
  ("флеровий", 2), ("московий", 2), ("ливерморий", 3), ("теннессин", 3), ("оганессон", 4)
  ]

rhymeClass :: (String, Int) -> String
rhymeClass (name, accent) = normalizeVowels (drop (accent - 1) (vowels name)) where
  normalizeVowels s = map (\c -> if c == 'е' then 'э' else c) s
guessedRhymeClasses = mapM (\g -> putStrLn (show g ++ ",")) nonEmptyGroups where
  multiSyllabic = filter (\p -> syllableCount (fst p) > 1) (M.toList elementsWithAccents)
  allGroups = M.elems $ M.fromListWith (++) (map (\p -> (rhymeClass p, [fst p])) multiSyllabic)
  nonEmptyGroups = filter (\g -> length g > 1) allGroups

postProcessedRhymes = [
 -- finishing
  ["лантан","титан", "уран"],
  ["неодим","празеодим"],
  ["оганессон","радон","ксенон","криптон","аргон","неон"],
  ["кислород","азот","углерод","водород", "йод"],
  ["золото","олово"],
  ["молибден","селен"],
  ["фтор", "хлор", "бор"],
  ["бром", "хром"],

 -- non-finishing
  ["дармштадтий","радий","палладий","ванадий","скандий"],
  ["самарий","барий","натрий"],
  ["галлий","калий","таллий"],
  ["франций","кальций"],
  ["гафний","кадмий","магний"],
  ["коперниций","америций"],
  ["протактиний","актиний","гадолиний","алюминий"],
  ["иридий","индий","рубидий","литий"],
  ["ливерморий","борий","торий"],
  ["московий","флеровий"],
  ["нихоний","калифорний","плутоний","полоний","цирконий"],
  ["осмий","гольмий"],
  ["дубний","нептуний","тулий"],
  ["рентгений","эйнштейний","рений","рутений","кремний","гелий"],
  ["иттербий","эрбий","тербий"],
  ["нобелий","берклий"]
  ]

template = [
  (["Есть"], ["кислород", "и", "углерод", "и", "водород", "и", "йод"]), -- manual alliteration
  (["а также"], ["азот"]), 
  ([], ["лантан"]),
  (["и"], ["титан"]),
  ([], ["ванадий", "и"]),
  (["вдобавок"], ["радий", "и"]),
  (["ещё"], ["радон"]),

  ([], ["оганессон", "."]),

  (["Ещё есть"], ["рубидий", "и"]),
  (["вдобавок"], ["литий", "и"]),
  ([], ["фтор"]),
  (["и"], ["хлор", "."]),
  
  (["Есть"], ["дубний", "и"]),
  ([], ["нептуний", "и"]),
  (["ещё"], ["ксенон"]),
  (["и"], ["криптон"]),
  (["а также"], ["плутоний", "и"]),
  (["ещё"], ["цирконий", "и"]),
  (["вдобавок"], ["аргон"]),

  (["а также"], ["неон", "."]),

  (["Есть"], ["калий", "и"]),
  ([], ["таллий", "и"]),
  (["есть"], ["бром"]),
  (["и"], ["хром", "."])
 ]
  
remainingWords = filter (\w -> not $ S.member w templateWords) elements
templateWords = S.fromList $ concatMap (\p -> fst p ++ snd p) template
remainingShapes = map shape remainingWords

-----------------------

data WordShape = WordShape { wsTotal :: Int, wsAccent :: Int } deriving (Show, Eq, Ord)

shape word = WordShape (syllableCount word) (M.findWithDefault (-1) word elementsWithAccents)
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

markLine start end = filter fillersSoundNice $ inner 0 start where
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

type ShapeVector = Word64

vectorLength = 8

toVector :: [WordShape] -> ShapeVector
toVector shapes = listToVector $ map (\i -> M.findWithDefault 0 i index2Count) [0..vectorLength - 1] where
  index2Count = M.fromListWith (+) $ map (\s -> (shapeIndex s, 1)) $ filter (/= fillerShape) shapes
  shapeIndex :: WordShape -> Int
  shapeIndex shape = (wsTotal shape) * (wsTotal shape - 1) `div` 2 + (wsTotal shape - wsAccent shape)
  listToVector :: [Int] -> ShapeVector
  listToVector list = sum $ zipWith (\val index -> shift (fromIntegral val) (index * 8)) list [0..vectorLength - 1]
  
templateVectors = map (\lineMarkups -> removeDuplicates id $ map toVector lineMarkups) templateMarkups

removeDuplicates by xs = inner S.empty xs where
  inner _ [] = []
  inner visited (x:xs) = let eq = by x in
    if eq `S.member` visited then inner visited xs else x:(inner (S.insert eq visited) xs)

-------------

type SumLine = [(ShapeVector, ShapeVector)]

solvePart :: [[ShapeVector]] -> ShapeVector -> Maybe [ShapeVector] 
solvePart lineVectors targetVector = getPath (length table - 1) targetVector [] where
  table = map addSumLine $ zip ([(0,0)]:table) lineVectors
  addSumLine :: (SumLine, [ShapeVector]) -> SumLine
  addSumLine (prev, vs) = removeDuplicates fst $ filter (bounded . fst) [(fst prevSum + v, v) | prevSum <- prev, v <- vs]
  bounded vec = all (\i -> vecByte vec i <= vecByte targetVector i) [0..vectorLength - 1]
  vecByte :: ShapeVector -> Int -> Word64 
  vecByte vec i = shift vec (-i * 8) .&. 255
  getPath index sum result =
    if index < 0 then Just result
    else case lookup sum (table !! index) of
      Just vec -> getPath (index - 1) (sum - vec) (vec : result)
      Nothing -> Nothing 

template1 = take 12 templateVectors
template2 = drop 12 templateVectors
target1 = toVector $ take (length remainingShapes `div` 2) remainingShapes
target2 = toVector $ drop (length remainingShapes `div` 2) remainingShapes

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
      let word = if eachShape == fillerShape then "и" else bestMatching c $ filter (\w -> shape w == eachShape) $ S.elems availableWords 
      in (S.delete word availableWords, result ++ [word])

  rating line words = homogeneity (words ++ snd (template !! line))

  mapping :: M.Map Int [String]
  mapping = fillLines (S.fromList remainingWords) M.empty where
    fillLines availableWords result = answer where
      options = map (\i -> (i, fillLine availableWords i)) $ filter (not . (`M.member` result)) lineIndices
      (bestLine, bestWords) = maximumBy (comparing $ \(line, words) -> rating line words) $ reverse options
      answer = if null options then result 
               else fillLines (S.difference availableWords $ S.fromList bestWords) (M.insert bestLine bestWords result)

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
fixPhoneticIssue lines loc = find isBetter $ sortBy (comparing (negate . homogeneity . concat)) allSwaps where
  isBetter ls = length (findPhoneticIssues ls) < issueCount where
    issueCount = length (findPhoneticIssues lines)
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
    w1:"и":_ -> False
    "и":_ -> False
    ".":_ -> False
    _ -> feed == "\n"
  suffixes = zipWith3 (\feed comma nextWord -> comma ++ (if nextWord == "." || nextWord == "," || feed == "\n" then feed else " ")) lineFeeds commas (drop 1 flatWords)

{-
Варианты концовки: 

1. всем этим элементам в институтах обучают
2. наверно, есть другие, только их пока не знают 
| а сколько их ещё, пока учёные не знают
   
1. и это все известные (в гарвАрдах|науке|учёным) элементы 
| здесь то, что я (нагуглил|найти смог) в этих ваших интернетах
| здесь только элементы, что нашёл я в интернете
| здесь все сто восемнадцать нам известных элементов
| и это только те, что знают в университетах
2. простите, у меня для вас других пока что нету

1. здесь только элементы, что ученые нарыли
2. есть много и других, но их пока что не открыли

---------
рифмы?
институты, университеты, Гарвард, ученые, нарыли, знакомы, учат, в программе, объявили, обучают, бывает, считают
открыли, открыты, известны, исследованы, обнаружены, синтезированы, найдены, не знаем, не знают

-}