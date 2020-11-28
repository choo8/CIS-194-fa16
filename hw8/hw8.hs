import Data.Char
import Data.Maybe
import System.Environment
import System.IO
import System.Exit

-- Exercise 1

data ComplicatedA a b
    = Con1 a b
    | Con2 [Maybe (a -> b)]

data ComplicatedB f g a b
    = Con3 (f a)
    | Con4 (g b)
    | Con5 (g (g [b]))

instance Functor (ComplicatedA a) where
    fmap f (Con1 a x) = Con1 a (f x)
    fmap f (Con2 xs) = Con2 (map go xs)
        where go Nothing = Nothing
              go (Just f') = Just (\x -> f . f' $ x)

instance Functor g => Functor (ComplicatedB f g a) where
    fmap f (Con3 fa) = Con3 fa
    fmap f (Con4 gb) = Con4 (fmap f gb)
    fmap f (Con5 ggbs) = Con5 (fmap (fmap (map f)) ggbs)

-- Exercise 2

func0 :: Monad f => (a -> a) -> f a -> f a
func0 f xs = do
    x <- xs
    return (f (f x))

func0' :: Functor f => (a -> a) -> f a -> f a
func0' f xs = (f . f) <$> xs

func1 :: Monad f => f a -> f (a,a)
func1 xs = xs >>= (\x -> return (x,x))

func1' :: Functor f => f a -> f (a,a)
func1' xs = (\x -> (x,x)) <$> xs

func2 :: Monad f => f a -> f (a,a)
func2 xs = xs >>= (\x -> xs >>= \y -> return (x,y))

func2' :: Applicative f => f a -> f (a,a)
func2' xs = (,) <$> xs <*> xs

func3 :: Monad f => f a -> f (a,a)
func3 xs = xs >>= (\x -> xs >>= \y -> return (x,x))

func3' :: Applicative f => f a -> f (a,a)
func3' xs = (\x _ -> (x,x)) <$> xs <*> xs

func4 :: Monad f => f a -> f a -> f (a,a)
func4 xs ys = xs >>= (\x -> ys >>= \y -> return (x,y))

func4' :: Applicative f => f a -> f a -> f (a,a)
func4' xs ys = (,) <$> xs <*> ys

func5 :: Monad f => f Integer -> f Integer -> f Integer
func5 xs ys = do
    x <- xs
    let x' = x + 1
    y <- (+1) <$> ys
    return (x' + y)

func5' :: Applicative f => f Integer -> f Integer -> f Integer
func5' xs ys = (\x y -> (x+1) + (y+1)) <$> xs <*> ys

func6 :: Monad f => f Integer -> f (Integer,Integer)
func6 xs = do
    x <- xs
    return $ if x > 0 then (x, 0)
                      else (0, x)

func6' :: Functor f => f Integer -> f (Integer,Integer)
func6' xs = fmap (\x -> if x > 0 then (x,0) else (0,x)) xs

func7 :: Monad f => f Integer -> f (Integer,Integer)
func7 xs = do
    x <- xs
    if x > 0 then return (x, 0)
             else return (0, x)

-- func7 cannot be implemented without Monad if we care about the precise evaluation and laziness behaviour
-- because if we return the statement in func6, it need not be evaluated in certain functions
-- e.g. if the Monad is Maybe, then the function "isJust" does not need to evaluate the Monadic value wrapped inside the Monad

func8 :: Monad f => f Integer -> Integer -> f Integer
func8 xs x = pure (+) <*> xs <*> pure x

func8' :: Functor f => f Integer -> Integer -> f Integer
func8' xs x = (+x) <$> xs

func9 :: Monad f => f Integer -> f Integer -> f Integer -> f Integer
func9 xs ys zs = xs >>= \x -> if even x then ys else zs

-- func9 cannot be implemented without Monad as the computation depends on the result of first argument

func10 :: Monad f => f Integer -> f Integer
func10 xs = do
    x <- xs >>= (\x -> return (x * x))
    return (x + 10)

func10' :: Functor f => f Integer -> f Integer
func10' xs = (\x -> (x * x) + 10) <$> xs

-- Exercise 3

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content

data Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = case (runParser p) s of
                 Just (x, "") -> Just x
                 _ -> Nothing

noParser :: Parser a
noParser = P (\_ -> Nothing)

pureParser :: a -> Parser a
pureParser x = P (\s -> Just (x,s))

instance Functor Parser where
    fmap f p = P (\s -> case (runParser p) s of
                             Nothing -> Nothing
                             Just (x, s') -> Just (f x, s'))

instance Applicative Parser where
    pure = pureParser
    fp <*> fx = P (\s -> case (runParser fp) s of
                              Nothing -> Nothing
                              Just (f, s') -> case (runParser fx) s' of
                                              Nothing -> Nothing
                                              Just (x, s'') -> Just (f x, s''))

instance Monad Parser where
    return = pureParser
    fa >>= k = P (\s -> case (runParser fa) s of
                             Nothing -> Nothing
                             Just (x, s') -> runParser (k x) s')

anyChar :: Parser Char
anyChar = P (\s -> case s of
                   [] -> Nothing
                   (x:s') -> Just (x,s'))

char :: Char -> Parser ()
char c = anyChar >>= (\c' -> if c == c' then return () else noParser)

anyCharBut :: Char -> Parser Char
anyCharBut c = anyChar >>= (\c' -> if c == c' then noParser else return c') 

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 = P (\s -> case (runParser p1) s of
                             Just r -> Just r
                             Nothing -> (runParser p2) s)

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> (many p)) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p1 p2 = ((:) <$> p1 <*> (many (p2 >> p1))) `orElse` return []

-- Exercise 4

type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = many1 parseSection
    where parseSection = do
            char '['
            i <- parseIdent
            char ']'
            decs <- many parseLine
            return (i, catMaybes decs)
          parseIdent = many1 letterOrDigit
          parseDec = do
            id <- parseIdent
            many (char ' ')
            char '='
            many (char ' ')
            val <- many1 (anyCharBut '\n')
            char '\n'
            return (Just (id, val))
          parseComment = do
            char '#'
            many1 (anyCharBut '\n')
            char '\n'
            return Nothing
          parseEmpty = do
            char '\n'
            return Nothing
          parseLine = parseDec `orElse` parseComment `orElse` parseEmpty       

letterOrDigit :: Parser Char
letterOrDigit = do
    c <- anyChar
    if isAlphaNum c then return c else noParser

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

main :: IO ()
main = do
    args <- getArgs
    input <- case args of
        [] -> getContents
        [fileName] -> readFile fileName
        _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
    case parse parseINI input of
        Just i -> print i
        Nothing -> do
            hPutStrLn stderr "Failed to parse INI file."
            exitFailure