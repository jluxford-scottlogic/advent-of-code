module MoveArr where

data MoveList a = MoveList [a] a [a]

instance Functor MoveList where
    fmap f (MoveList sa a as) = MoveList (map f sa) (f a) (map f as)

instance Show a => Show (MoveList a) where
    show (MoveList sa a as) = "{" ++ (show (reverse sa)) ++ " " ++ (show a) ++ " " ++ (show as) ++ "}"

instance Foldable MoveList where
    foldr f z (MoveList sa a as) = (foldr f z (reverse (sa ++ a:as)))

instance Traversable MoveList where
    traverse f (MoveList sa a as) = MoveList <$> traverse f sa <*> f a <*> traverse f as

loc :: MoveList a -> Int
loc (MoveList sa a as) = length sa

moveListFromList :: [a] -> MoveList a
moveListFromList (c:cs) = MoveList [] c cs

moveListToList :: MoveList a -> [a]
moveListToList (MoveList sa a as) = reverse sa ++ (a:as)

step :: Int -> MoveList a -> Maybe (MoveList a)
step 0 m = Just m
step n m@(MoveList [] x (a:as)) | n > 0 = step (n - 1) (MoveList [x] a as)
                                | n < 0 = Nothing
step n m@(MoveList (s:sa) x []) | n > 0 = Nothing
                                | n < 0 = step (n + 1) (MoveList sa s [x])
step n (MoveList (s:sa) x (a:as)) | n > 0 = step (n - 1) (MoveList (x:s:sa) a as)
                                  | n < 0 = step (n + 1) (MoveList (sa) s (x:a:as))

peek :: Int -> MoveList a -> Maybe a
peek 0 (MoveList _ a _) = Just a
peek n ms = (step n ms) >>= peek 0

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

stepA :: Axis -> Int -> MoveArr a -> Maybe (MoveArr a)
stepA Y n (Arr ms) = fmap Arr (step n ms)
stepA X n (Arr ms) = fmap Arr . sequence $ (fmap (step n) ms)

stepThrough :: MoveArr a -> Maybe (MoveArr a)
stepThrough arr@(Arr (MoveList ssa (MoveList sa a []) [])) = Nothing
stepThrough arr@(Arr (MoveList ssa (MoveList sa a []) ass)) = (stepA X (-(length sa)) arr) >>= (stepA Y 1)
stepThrough arr = stepA X 1 arr

atEnd :: MoveArr a -> Bool
atEnd (Arr (MoveList ssa (MoveList sa a []) [])) = True
atEnd _ = False

peekArr :: Axis -> Int -> MoveArr a -> Maybe a
peekArr _ 0 (Arr (MoveList _ (MoveList _ x _) _)) = Just x
peekArr a n ms = (stepA a n ms) >>= (peekArr a 0)

stepXY :: (Int, Int) -> MoveArr a -> Maybe (MoveArr a)
stepXY (x,y) arr = (stepA X x arr) >>= (stepA Y y)

peekXY :: (Int, Int) -> MoveArr a -> Maybe a
peekXY (x,y) arr = (stepA X x arr) >>= (peekArr Y y)

updateLoc :: (a -> a) -> MoveArr a -> MoveArr a
updateLoc f (Arr (MoveList ssa (MoveList sa a as) ass)) = Arr (MoveList ssa (MoveList sa (f a) as) ass)