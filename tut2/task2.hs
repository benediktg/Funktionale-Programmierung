threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c
    | (a == b) && (b == c) = True
    | otherwise = False

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d
    | (c == d) && threeEqual a b c = True
    | otherwise = False

fourEqual' :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d
    | (a == b) && (b == c) && (c == d) = True
    | otherwise = False

xor :: Bool -> Bool -> Bool
xor a b =
    (a || b) && not (a && b)

xor3 :: Bool -> Bool -> Bool -> Bool
xor3 a b c =
    xor (xor a b) c

xor3' :: Bool -> Bool -> Bool -> Bool
xor3' a b c =
    if c == False
       then (a || b) && not (a && b)
       else (a && b) || not (a || b)
