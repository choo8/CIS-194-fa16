-- Exercise 1

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 2

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList 

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f x = Cons x (streamIterate f (f x))

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

nats :: Stream Integer
nats = streamIterate (+1) 0

ruler :: Stream Integer
ruler = go 0
    where go n = streamInterleave (streamRepeat n) (go (n + 1))

-- Exercise 3

data Supply s a = S (Stream s -> (a, Stream s))

get :: Supply s s
get = S (\(Cons x xs) -> (x, xs))

pureSupply :: a -> Supply s a
pureSupply x = S (\xs -> (x, xs))

mapSupply :: (a -> b) -> Supply s a -> Supply s b
mapSupply f (S g) = S go
    where go s = let (a, s') = g s 
                 in (f a, s')

mapSupply2 :: (a -> b -> c) -> Supply s a -> Supply s b -> Supply s c
mapSupply2 f (S g) (S h) = S go
    where go s = let (a, s') = g s
                     (a', s'') = h s' 
                 in (f a a', s'')

bindSupply :: Supply s a -> (a -> Supply s b) -> Supply s b
bindSupply (S f) g = S go
    where go s = let (a, s') = f s
                     (S h) = g a
                     (b, s'') = h s'
                 in (b, s'')

runSupply :: Stream s -> Supply s a -> a
runSupply s (S f) = fst . f $ s

instance Functor (Supply s) where
    fmap = mapSupply

instance Applicative (Supply s) where
    pure = pureSupply
    (<*>) = mapSupply2 id

instance Monad (Supply s) where
    return = pureSupply
    (>>=) = bindSupply

data Tree a = Node (Tree a) (Tree a) | Leaf a deriving Show

labelTree :: Tree a -> Tree Integer
labelTree t = runSupply nats (go t)
  where
    go :: Tree a -> Supply s (Tree s)
    go (Leaf _) = Leaf <$> get 
    go (Node t1 t2) = Node <$> go t1 <*> go t2