module English (elementsWithAccents, elements, syllableCount, template, conjunct) where

import qualified Data.Map as M

syllableCount "seaborgium" = 4
syllableCount "meitnerium" = 4
syllableCount "roentgenium" = 4
syllableCount s = length (filter (`elem` "aeiou") s)

elements = M.keys elementsWithAccents

elementsWithAccents :: M.Map String Int
elementsWithAccents = M.fromList [
    ("lawrencium", 2),
    ("rutherfordium", 3),
    ("dubnium", 1),
    ("seaborgium", 2),
    ("bohrium", 1),
    ("hassium", 1),
    ("meitnerium", 2),
    ("darmstadtium", 2),
    ("roentgenium", 2),
    ("copernicium", 3),
    ("nihonium", 2),
    ("flerovium", 2),
    ("moscovium", 2),
    ("livermorium", 3),
    ("tennessine", 1),
    ("oganesson", 3)]

conjunct = "and"

template = [
    ([], ["flerovium"]),
    ([], ["moscovium"]),
    ([], ["bohrium"]),
    ([], ["livermorium"])
    ]