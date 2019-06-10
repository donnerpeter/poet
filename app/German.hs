module German (elementsWithAccents, syllableCount, template, elements, postProcessedRhymes, conjunct, phonemes) where

import Data.List
import Data.Char
import qualified Data.Map as M

syllableCount s = length (vowels s)
isVowel p = p `isInfixOf` "AÄEIÖOUY" || p == "AU" || p == "AI" || p == "OI"
vowels s = filter isVowel (phonemes s)

phonemes :: String -> [String]
phonemes word = inner $ map toUpper word where 
    inner [] = []
    inner ('Q':'U':rest) = "KW" : inner rest
    inner ('A':'U':rest) = "AU" : inner rest
    inner ('E':'I':rest) = "AI" : inner rest
    inner ('E':'U':rest) = "OI" : inner rest
    inner ('S':'E':'A':rest) = "S" : "I" : inner rest -- Seaborgium
    inner ('A':'S':'T':rest) = "A" : "S" : "T" : inner rest
    inner ('S':'T':rest) = "Ŝ" : "T" : inner rest
    inner ('S':'C':'H':rest) = "Ŝ" : inner rest
    inner ('P':'H':rest) = "F" : inner rest
    inner ('X':rest) = "K" : "S" : inner rest
    inner ('C':'H':rest) = "H" : inner rest
    inner ('C':'K':rest) = "K" : inner rest
    inner ('C':v:rest) | v `elem` "AOU" = "K" : inner (v:rest)
    inner ('S':v:rest) | v `elem` "AOUEI" = "Z" : inner (v:rest)
    inner ('Z':rest) = "C" : inner rest
    inner (c:rest) = [c]:inner rest

conjunct = "und"

guessAccent s = let sc = syllableCount s in
  if sc == 1 then Just 1
  else if "ium" `isSuffixOf` s then Just (sc - 2)
  else Nothing

guessAccents = mapM f elements where
  f s = case guessAccent s of
    Just i -> putStrLn $ "\"" ++ s ++ "\", " ++ (show i)
    Nothing -> putStrLn $ "\"" ++ s ++ "\", "

elements = M.keys elementsWithAccents

-- 360 syllables, 384 to fill

elementsWithAccents :: M.Map String Int
elementsWithAccents = M.fromList [
    ("Actinium", 2), ("Aluminium", 3), ("Americium", 3), ("Antimon", 3), ("Argon", 2), -- 1?
    ("Arsen", 2), ("Astat", 2),
    ("Barium", 1), ("Berkelium", 2), ("Beryllium", 2), ("Bismut", 1), ("Blei", 1), ("Bohrium", 1), ("Bor", 1), ("Brom", 1),
    ("Cadmium", 1), ("Cäsium", 1), ("Calcium", 1), ("Californium", 3), ("Cer", 1), ("Chlor", 1), ("Chrom", 1), ("Cobalt", 1), ("Copernicium", 3), ("Curium", 1),
    ("Darmstadtium", 2), ("Dubnium", 1), ("Dysprosium", 2),
    ("Einsteinium", 2), ("Eisen", 1), ("Erbium", 1), ("Europium", 2),
    ("Fermium", 1), ("Flerovium", 2), ("Fluor", 1), ("Francium", 1),
    ("Gadolinium", 3), ("Gallium", 1), ("Germanium", 2), ("Gold", 1),
    ("Hafnium", 1), ("Hassium", 1), ("Helium", 1), ("Holmium", 1),
    ("Indium", 1), ("Iridium", 2),
    ("Jod", 1),
    ("Kalium", 1), ("Kohlenstoff", 1), ("Krypton", 1), ("Kupfer", 1),
    ("Lanthan", 2), ("Lawrencium", 2), ("Lithium", 1), ("Livermorium", 3), ("Lutetium", 2),
    ("Magnesium", 2), ("Mangan", 2), ("Meitnerium", 2), ("Mendelevium", 3), ("Molybdän", 3), ("Moscovium", 2),
    ("Natrium", 1), ("Neodym", 3), ("Neon", 1), ("Neptunium", 2), ("Nickel", 1), ("Nihonium", 2), ("Niob", 2), ("Nobelium", 2),
    ("Oganesson", 4), -- ?
    ("Osmium", 1),
    ("Palladium", 2), ("Phosphor", 1), ("Platin", 1), ("Plutonium", 2), ("Polonium", 2), ("Praseodym", 4), ("Promethium", 2), ("Protactinium", 3),
    ("Quecksilber", 1),
    ("Radium", 1), ("Radon", 2), -- 1?
    ("Rhenium", 1), ("Rhodium", 1), ("Röntgenium", 2), ("Rubidium", 2), ("Ruthenium", 2), ("Rutherfordium", 3),
    ("Samarium", 2), ("Sauerstoff", 1), ("Scandium", 1), ("Schwefel", 1), ("Seaborgium", 2), ("Selen", 2), ("Silber", 1), ("Silicium", 2), ("Stickstoff", 1), ("Strontium", 1),
    ("Tantal", 1), ("Technetium", 2), ("Tellur", 2), ("Tenness", 1), -- ?
    ("Terbium", 1), ("Thallium", 1), ("Thorium", 1), ("Thulium", 1), ("Titan", 2),
    ("Uran", 2),
    ("Vanadium", 2),
    ("Wasserstoff", 1), ("Wolfram", 1),
    ("Xenon", 1),
    ("Ytterbium", 2), ("Yttrium", 1),
    ("Zink", 1), ("Zinn", 1), ("Zirconium", 2) ]

rhymeClass :: (String, Int) -> [String]
rhymeClass (name, accent) = drop (accent - 1) (vowels name)
guessedRhymeClasses = mapM (\g -> putStrLn (show g ++ ",")) nonEmptyGroups where
  multiSyllabic = filter (\p -> syllableCount (fst p) > 1) (M.toList elementsWithAccents)
  allGroups = M.elems $ M.fromListWith (++) (map (\p -> (rhymeClass p, [fst p])) multiSyllabic)
  nonEmptyGroups = filter (\g -> length g > 1) allGroups

postProcessedRhymes = [
    --finishing
    ["Uran","Titan","Mangan","Lanthan"],
    ["Radon","Oganesson","Argon","Antimon"],
    ["Praseodym","Neodym"],
    ["Thulium","Neptunium","Dubnium","Curium"],
    ["Vanadium","Scandium","Radium","Darmstadtium","Palladium"],
    ["Thallium","Kalium","Gallium"],
    ["Samarium","Natrium","Barium"],
    ["Hafnium","Germanium"],
    ["Francium","Calcium"],
    ["Terbium","Erbium"],
    ["Röntgenium","Ruthenium","Rhenium"],
    ["Technetium","Promethium","Lutetium"],
    ["Nobelium","Berkelium","Helium"],
    ["Rubidium","Lithium","Iridium","Indium"],
    ["Silicium","Copernicium","Americium"],
    ["Protactinium","Gadolinium","Aluminium","Actinium"],
    ["Rutherfordium","Rhodium"],
    ["Polonium","Plutonium","Californium","Zirconium","Nihonium"],
    ["Thorium","Bohrium","Livermorium"],

    -- non-finishing
    ["Xenon","Neon"]
    ]

template = [
    (["Es gibt"], ["Rhenium"]),
    (["und"], ["Kohlenstoff", "und", "Wasserstoff", "und", "Stickstoff", "und", "Röntgenium"]),
    ([], ["Lanthan"]),
    (["und"], ["Titan"]),

    ([], ["Vanadium"]),
    (["und"], ["Radium"]),
    ([], ["Gallium"]),
    (["und"], ["Thallium"]),

    (["Es gibt"], ["Rubidium"]),
    (["und"], ["Iridium"]),
    ([], ["Samarium"]),
    (["und"], ["Barium"]),

    (["Es gibt"], ["Erbium"]),
    (["und"], ["Terbium"]),
    ([], ["Thorium"]),
    (["und"], ["Bohrium"]),

    (["und"], ["Plutonium"]),
    ([], ["Polonium"]),
    (["und"], ["Neptunium"]),
    ([], ["Dubnium"]),

    (["Es gibt"], ["Protactinium"]),
    ([], ["Gadolinium"]),
    ([], ["Aluminium"]),
    ([], ["Actinium"]),

    (["Es gibt"], ["Berkelium"]),
    (["und"], ["Nobelium"]),
    ([], ["Francium"]),
    ([], ["Calcium"])
    ]