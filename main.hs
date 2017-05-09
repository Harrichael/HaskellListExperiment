--ghc 7.10

import Data.List
import System.Random
import Control.Applicative

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
    getIx = getStdRandom $ randomR (1, length lst)
    pickElem n = case splitAt n lst of
        ([], s) -> error $ "failed at index " ++ show n -- should never match
        (r, s)  -> (last r, init r ++ s)

data List a = Empty | Link a (List a) | Concat (List a) (List a) | Append (List a) a

-- Why not abstract over List with a Tree, what do we lose?
data Tree a = Leaf a | Branch (Tree a) (Tree a)

{-
extend List a with
    Concat (List a) (List a)
    where
        Concat Empty ys       = ys
        Concat xs Empty       = xs
        Concat (Link x xs) ys = Link x (Concat xs ys)

extend List a with
    Append (List a) a
    where
        Append Empty y       = Link y Empty
        Append (Link x xs) y = Link x (Append xs y)

last' Empty             = Nothing
last' (Append xs y)     = Just y
last' (Concat xs Empty) = last' xs
last' (Concat xs ys)    = last' ys
last' (Link x Empty)    = Just x
last' (Link x xs)       = last' xs
-}

instance Show a => Show (List a) where
    show Empty            = "-|"
    show (Link x Empty)   = show x  ++ " -|"
    show (Link x xs)      = show x  ++ " -> " ++ show xs
    show (Append Empty x) = show x ++ " -|"
    show (Append xs x)    = show' xs ++ " -> " ++ show x ++ " -|"
        where
            show' Empty            = ""
            show' (Link x Empty)   = show x
            show' (Link x xs)      = show x ++ " -> " ++ show' xs
            show' (Concat xs ys)   = show' xs ++ " -> " ++ show' ys
            show' (Append Empty x) = show x
            show' (Append xs x)    = show' xs ++ " -> " ++ show x
    show (Concat xs ys)   = show' xs ++ " -> " ++ show ys
        where
            show' Empty            = ""
            show' (Link x Empty)   = show x
            show' (Link x xs)      = show x ++ " -> " ++ show' xs
            show' (Concat xs ys)   = show' xs ++ " -> " ++ show' ys
            show' (Append Empty x) = show x
            show' (Append xs x)    = show' xs ++ " -> " ++ show x

--extend List a where
--    Concat Empty ys           = ys
--    Concat xs    Empty        = xs
--    Concat (Link x xs)    ys) = Link x (Concat xs ys)
--    Concat (Append xs x)  ys) = Concat xs (Concat (Link x Empty) ys)
--    Concat (Concat xs zs) ys) = Concat xs (Concat zs ys)
--    Append (List a) a =

last' Empty              = Nothing
last' (Link   x   Empty) = Just x
last' (Link   x   xs)    = last' xs
last' (Concat xs  Empty) = last' xs
last' (Concat xs  ys)    = last' ys
last' (Append xs  x)     = Just x

convert' []     = Empty
convert' (x:xs) = Link x (convert' xs)

flatten' Empty                      = Empty
flatten' (Link x xs)                = Link x $ flatten' xs
flatten' (Concat Empty          ys) = flatten' ys
flatten' (Concat (Link x xs)    ys) = Link x $ flatten' (Concat xs ys)
flatten' (Concat (Append xs x)  ys) = flatten' $ Concat xs $ Concat (Link x Empty) ys
flatten' (Concat (Concat xs zs) ys) = flatten' $ Concat xs $ Concat zs ys
flatten' (Append xs x)              = flatten' $ Concat xs (Link x Empty)

quickSort []     = []
quickSort (x:xs) = lower ++ [x] ++ upper
    where
        lower = quickSort $ filter (<  x) xs
        upper = quickSort $ filter (>= x) xs

quickSort' []     = Empty
quickSort' (x:xs) = Concat (Append lower x) upper
    where
        lower = quickSort' $ filter (<  x) xs
        upper = quickSort' $ filter (>= x) xs

main = do
    --let els = Append (Concat (Concat (Append (convert' "Hello") '@') (convert' " World")) $ convert' "!!") $ 'x'
    --print els
    --print ( flatten' els )
    els <- shuffle [1..10000]
    
    --print ( flatten' . quickSort' els )
    --print ( convert' . quickSort els )
    --print ( convert' . sort els )
    print ( sort els )
