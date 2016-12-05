-- a)
data FloatExpr = Val Float
               | Add FloatExpr FloatExpr
               | Mul FloatExpr FloatExpr
               | Sqrt FloatExpr
               | Abs FloatExpr

-- b)
numOps :: FloatExpr -> Int
numOps (Val x) = 0
numOps (Add x y) = 1 + (numOps x) + (numOps y)
numOps (Mul x y) = 1 + (numOps x) + (numOps y)
numOps (Sqrt x) = 1 + numOps x
numOps (Abs x) = 1 + numOps x

numVals :: FloatExpr -> Int
numVals (Val x) = 1
numVals (Add x y) = (numVals x) + (numVals y)
numVals (Mul x y) = (numVals x) + (numVals y)
numVals (Sqrt x) = numVals x
numVals (Abs x) = numVals x

-- c)
evalExpr :: FloatExpr -> Float
evalExpr (Val x) = x
evalExpr (Add x y) = (evalExpr x) + (evalExpr y)
evalExpr (Mul x y) = (evalExpr x) * (evalExpr y)
evalExpr (Sqrt x) = sqrt (evalExpr x)
evalExpr (Abs x) = abs (evalExpr x)

-- d)
showExpr :: FloatExpr -> String
showExpr (Val x) = show x
showExpr (Add x y) = "(" ++ (showExpr x) ++ ") + (" ++ (showExpr y) ++ ")"
showExpr (Mul x y) = "(" ++ (showExpr x) ++ ") * (" ++ (showExpr y) ++ ")"
showExpr (Sqrt x) = "sqrt(" ++ (showExpr x) ++ ")"
showExpr (Abs x) = "abs(" ++ (showExpr x) ++ ")"
