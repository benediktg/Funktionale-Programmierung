calcFibonacci :: Integer -> Integer
calcFibonacci n =
    if (n <= 2) then 1
       else calcFibonacci (n - 1) + calcFibonacci (n - 2)
