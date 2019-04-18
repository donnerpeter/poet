module Russian (elementsWithAccents, syllableCount, template, elements, postProcessedRhymes, conjunct) where

import qualified Data.Map.Strict as M
import Data.List

isVowel c = c `elem` "аЕеёиоуыэюя"
syllableCount s = length (vowels s)
vowels s = filter isVowel s

conjunct = "и"

guessAccent s = let sc = syllableCount s in
  if sc == 1 then Just 1
  else if "ий" `isSuffixOf` s then Just (sc - 1)
  else Nothing

guessAccents = mapM f elements where
  f s = case guessAccent s of
    Just i -> putStrLn $ "\"" ++ s ++ "\", " ++ (show i)
    Nothing -> putStrLn $ "\"" ++ s ++ "\", "

elements = M.keys elementsWithAccents

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

similarityClasses = [
  ["германий", "галлий", "индий", "рутений", "европий", "франций", "америций", "калифорний", "теннессин", "скандий", "полоний", "хассий", "нихоний"],
  ["берклий", "дармштадтий", "московий", "дубний", "ливерморий"],
  ["кюрий", "эйнштейний", "фермий", "менделевий", "нобелий", "лоуренсий", "резерфордий", "борий", "мейтнерий", "рентгений", "коперниций", "оганессон", "флеровий", "сиборгий"],
  ["иттербий", "эрбий", "тербий", "иттрий"],
  ["селен", "уран", "нептуний", "плутоний"]
  ] ++ filter (\l -> length l >= 3) (postProcessedRhymes ++ M.elems (M.fromListWith (++) $ map (\w -> (head w, [w])) elements))

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