import Data.Char
import Data.List

-- Exercise 1

halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

safeString :: String -> String
safeString = map (\x -> if isControl x || not (isAscii x) then '_' else x)

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

holes :: [a] -> [[a]]
holes xs = map (\x -> removeN (fst x) (snd x)) . zip [1..n] . take n . repeat $ xs
    where n = length xs
          removeN n xs = take (n - 1) xs ++ (drop n xs)

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

longestText :: Show a => [a] -> a
longestText = snd . foldl1 (\acc x -> if fst x >= fst acc then x else acc) . map (\x -> (length (show x), x))

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"
   ]

adjacents :: [a] -> [(a,a)]
adjacents xs = zipWith (,) first second
   where n = length xs
         first = take (n - 1) xs
         second = drop 1 xs

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

commas :: [String] -> String
commas = intercalate ", "

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = foldl1 (\x y -> zipWith (+) x y)

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

sumNumbers :: String -> Integer
sumNumbers = fst . foldr (\x acc -> let n = fst acc
                                        cur = snd acc
                                    in if isDigit x then (n, x : cur) else if cur == "" then (n, cur) else (n + (read cur), "")) (0, "") 
 
ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]

-- Exercise 2

wordCount :: String -> String
wordCount xs = output
   where ls = lines xs
         empty = length . filter (\x -> x == "") $ ls
         ws = words $ xs
         unique = nub ws
         numPrev = length . filter (uncurry (==)) $ (adjacents ws)
         longest = maximum . map length $ ls
         output = "Number of lines: " 
                ++ show (length ls)
                ++ "\nNumber of empty lines: "
                ++ show empty
                ++ "\nNumber of words: "
                ++ show (length ws)
                ++ "\nNumber of unique words: "
                ++ show (length unique)
                ++ "\nNumber of words follows by themselves: "
                ++ show numPrev
                ++ "\nLength of longest line: "
                ++ show longest

-- Exercise 3

testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens)
              , ("safeString",      ex_safeString)
              , ("holes",           ex_holes)
              , ("longestText",     ex_longestText)
              , ("adjacents",       ex_adjacents)
              , ("commas",          ex_commas)
              , ("addPolynomials",  ex_addPolynomials)
              , ("sumNumbers",     ex_sumNumbers)
              ]

formatTests :: [(String, [Bool])] -> String
formatTests = unlines . map (\x -> let ex = fst x
                                       res = snd x
                                       correct = filter id $ res
                                   in ex ++ ": " ++ show (length correct) ++ "/" ++ show (length res) ++ " successful tests.")

main :: IO ()
main = interact wordCount