nand :: Bool -> Bool -> Bool
nand a b =
    if a && b
       then False
       else True

nand' :: Bool -> Bool -> Bool
nand' a b
    | a && b = False
    | otherwise = True

