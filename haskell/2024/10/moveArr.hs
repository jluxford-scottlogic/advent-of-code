module MoveArr where

data MoveList a = MoveList [a] a [a]

instance Functor MoveList where
    fmap f (MoveList sa a as) = MoveList (map f sa) (f a) (map f as)

instance Show a => Show (MoveList a) where
    show (MoveList sa a as) = "{" ++ (show (reverse sa)) ++ " " ++ (show a) ++ " " ++ (show as) ++ "}"


loc :: MoveList a -> Int
loc (MoveList sa a as) = length sa

moveListFromList :: [a] -> MoveList a
moveListFromList (c:cs) = MoveList [] c cs

moveListToList :: MoveList a -> [a]
moveListToList (MoveList sa a as) = reverse sa ++ (a:as)

step :: Int -> MoveList a -> MoveList a
step 0 m = m
step n m@(MoveList [] x (a:as)) | n > 0 = step (n - 1) (MoveList [x] a as)
                                | n < 0 = m
step n m@(MoveList (s:sa) x []) | n > 0 = m
                                | n < 0 = step (n + 1) (MoveList sa s [x])
step n (MoveList (s:sa) x (a:as)) | n > 0 = step (n - 1) (MoveList (x:s:sa) a as)
                                  | n < 0 = step (n + 1) (MoveList (sa) s (x:a:as))

peek :: Int -> MoveList a -> a
peek 0 (MoveList _ a _) = a
peek n ms = peek 0 (step n ms)

data MoveArr a = Arr (MoveList (MoveList a))
    deriving Show

instance Functor MoveArr where
    fmap f (Arr ms) = Arr (fmap (fmap f) ms)

data Axis = X | Y

locArr :: Axis -> MoveArr a -> Int
locArr Y (Arr arr) = loc arr
locArr X (Arr (MoveList _ x _)) = loc x

fromArr :: [[a]] -> MoveArr a
fromArr (cs:css) = Arr (MoveList [] (moveListFromList cs) (fmap moveListFromList css))

toArr :: MoveArr a -> [[a]]
toArr (Arr (MoveList ssa as ass)) = moveListToList (MoveList (fmap moveListToList ssa) (moveListToList as) (fmap moveListToList ass))

stepA :: Axis -> Int -> MoveArr a -> MoveArr a
stepA Y n (Arr ms) = Arr (step n ms)
stepA X n (Arr ms) = Arr (fmap (step n) ms)

stepThrough :: MoveArr a -> MoveArr a
stepThrough arr@(Arr (MoveList ssa (MoveList sa a []) [])) = arr
stepThrough arr@(Arr (MoveList ssa (MoveList sa a []) ass)) = stepA Y 1 (stepA X (-(length sa)) arr)
stepThrough arr = stepA X 1 arr

atEnd :: MoveArr a -> Bool
atEnd (Arr (MoveList ssa (MoveList sa a []) [])) = True
atEnd _ = False

peekArr :: Axis -> Int -> MoveArr a -> a
peekArr _ 0 (Arr (MoveList _ (MoveList _ x _) _)) = x
peekArr a n ms = peekArr a 0 (stepA a n ms)

updateLoc :: (a -> a) -> MoveArr a -> MoveArr a
updateLoc f (Arr (MoveList ssa (MoveList sa a as) ass)) = Arr (MoveList ssa (MoveList sa (f a) as) ass)

lengthArr :: Axis -> MoveArr a -> Int
lengthArr Y (Arr (MoveList ssa as ass)) = length ssa + length ass + 1
lengthArr X (Arr (MoveList _ (MoveList sa a as) _)) = length sa + length as + 1

peekAround :: ([a] -> b) -> MoveArr a -> b
peekAround f xss = f [peekArr X 1 xss, peekArr X (-1) xss, peekArr Y 1 xss, peekArr Y (-1) xss]