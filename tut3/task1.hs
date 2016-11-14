orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c)
    | a > b = orderTriple (b, a, c)
    | b > c = orderTriple (a, c, b)
    | otherwise = (a, b, c)

rotateQuadruple :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
rotateQuadruple (a, b, c, d) n
    | n > 0 = rotateQuadruple (d, a, b, c) (n - 1)
    | n < 0 = rotateQuadruple (b, c, d, a) (n + 1)
    | otherwise = (a, b, c, d)

calcAddSubMulDiv :: Float -> Float -> (Float, Float, Float, Float)
calcAddSubMulDiv a b = (a + b, a - b, a * b, a / b)
