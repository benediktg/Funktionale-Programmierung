class (Visible a) where
    toString :: a -> String
    size :: a -> Int

-- a)
instance (Visible a, Visible b) => Visible (a, b) where
    toString (x, y) = "(" ++ (toString x) ++ ", " ++ (toString y) ++ ")"
    size (x, y) = (size x) + (size y)

-- b)
data (Tree a) = Leaf a
              | Branch (Tree a) (Tree a)

instance (Visible a) => Visible (Tree a) where
    toString (Leaf x) = toString x
    toString (Branch b1 b2) = (toString b1) ++ "-+-" ++ (toString b2)
    size (Leaf x) = 0
    size (Branch xs ys) = foldl (max) 0 (map size (xs ++ ys))  -- error
