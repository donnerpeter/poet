module German where

import Data.List
import qualified Data.Map as M

syllableCount s = length (vowels s)
isVowel p = p `isInfixOf` "AEIOUYauäeiöoy" || p == "Ei"
vowels s = filter isVowel (phonemes s)

phonemes :: String -> [String]
phonemes [] = []
 -- todo lowercase
phonemes ('Q':'u':rest) = "kw" : phonemes rest
phonemes ('a':'u':rest) = "au" : phonemes rest
phonemes ('E':'i':rest) = "ei" : phonemes rest
phonemes ('e':'i':rest) = "ei" : phonemes rest
phonemes ('A':'s':'t':rest) = "A" : "s" : "t" : phonemes rest
phonemes ('s':'t':rest) = "sch" : "t" : phonemes rest
phonemes ('c':'h':rest) = "xx" : phonemes rest
phonemes ('c':'k':rest) = "k" : phonemes rest
phonemes ('c':v:rest) | v `elem` "aou" = "k" : phonemes (v:rest)
phonemes ('z':rest) = "c" : phonemes rest
phonemes (c:rest) = [c]:phonemes rest

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
    ("Barium", 1), ("Berkelium", 2), ("Beryllium", 1), ("Bismut", 1), ("Blei", 1), ("Bohrium", 1), ("Bor", 1), ("Brom", 1),
    ("Cadmium", 1), ("Cäsium", 1), ("Calcium", 1), ("Californium", 3), ("Cer", 1), ("Chlor", 1), ("Chrom", 1), ("Cobalt", 1), ("Copernicium", 3), ("Curium", 1),
    ("Darmstadtium", 2), ("Dubnium", 1), ("Dysprosium", 1),
    ("Einsteinium", 4), ("Eisen", 1), ("Erbium", 1), ("Europium", 3),
    ("Fermium", 1), ("Flerovium", 2), ("Fluor", 1), ("Francium", 1),
    ("Gadolinium", 3), ("Gallium", 1), ("Germanium", 2), ("Gold", 1),
    ("Hafnium", 1), ("Hassium", 1), ("Helium", 1), ("Holmium", 1),
    ("Indium", 1), ("Iridium", 2),
    ("Jod", 1),
    ("Kalium", 1), ("Kohlenstoff", 1), ("Krypton", 1), ("Kupfer", 1),
    ("Lanthan", 2), ("Lawrencium", 2), ("Lithium", 1), ("Livermorium", 3), ("Lutetium", 2),
    ("Magnesium", 2), ("Mangan", 2), ("Meitnerium", 3), ("Mendelevium", 3), ("Molybdän", 3), ("Moscovium", 2),
    ("Natrium", 1), ("Neodym", 3), ("Neon", 1), ("Neptunium", 2), ("Nickel", 1), ("Nihonium", 2), ("Niob", 2), ("Nobelium", 2),
    ("Oganesson", 4), -- ? ("Osmium", 1),
    ("Palladium", 2), ("Phosphor", 1), ("Platin", 1), ("Plutonium", 2), ("Polonium", 2), ("Praseodym", 4), ("Promethium", 2), ("Protactinium", 3),
    ("Quecksilber", 1),
    ("Radium", 1), ("Radon", 2), -- 1?
    ("Rhenium", 1), ("Rhodium", 1), ("Röntgenium", 2), ("Rubidium", 2), ("Ruthenium", 2), ("Rutherfordium", 3),
    ("Samarium", 2), ("Sauerstoff", 1), ("Scandium", 1), ("Schwefel", 1), ("Seaborgium", 3), ("Selen", 2), ("Silber", 1), ("Silicium", 2), ("Stickstoff", 1), ("Strontium", 1),
    ("Tantal", 1), ("Technetium", 2), ("Tellur", 2), ("Tenness", 1), -- ?
    ("Terbium", 1), ("Thallium", 1), ("Thorium", 1), ("Thulium", 1), ("Titan", 2),
    ("Uran", 2),
    ("Vanadium", 2),
    ("Wasserstoff", 1), ("Wolfram", 1),
    ("Xenon", 1),
    ("Ytterbium", 1), ("Yttrium", 1),
    ("Zink", 1), ("Zinn", 1), ("Zirconium", 2) ]
