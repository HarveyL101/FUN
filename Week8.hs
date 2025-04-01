import Distribution.Simple.Program.HcPkg (list)
helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

displayFile :: IO ()
displayFile = do 
    putStr "Enter the filename: "
    name <- getLine
    contents <- readFile name
    putStr contents

getInt :: IO Int
getInt = do 
    str <- getLine
    return (read str :: Int)

isPalindrome :: String -> String
isPalindrome str
   | str == reverse str  = str ++ " is a palindrome"
   | otherwise           = str ++ " is not a palindrome"

pal :: IO ()
pal = do 
    line <- getLine
    let response = isPalindrome line
    putStrLn response

palLines :: IO ()
palLines = do 
    putStr "Enter a line: "
    str <- getLine
    if str == "" then 
        return ()
    else do 
        putStrLn (isPalindrome str)
        palLines

{-
  WORKED EXAMPLE 1:

  Exercise: Write a program guessingGame with the signature below. The program takes
  an integer as a parameter and asks the user to enter a guess it repeatedly until they have
  guessed correctly. After every guess, the program needs to tell the user whether their number
  was too large or too small.

  guessingGame :: Int -> IO ()
-}
guessingGame :: Int -> IO ()
guessingGame secret = do             -- Prompt user input, read it and then convert it to an Int data type
  putStrLn "Enter a number: "
  guess <- getLine
  let guessInt = read guess :: Int
  if guessInt == secret              -- If the users input is the same as 'secret' display a success message
    then putStrLn "You Guessed it!"
  else do 
    if guessInt < secret             -- Otherwise, notify them if the number is bigger or smaller than 
      then putStrLn "Too small!"     -- 'secret' and repeat the process by recursively calling guessingGame
      else putStrLn "Too big!"
    guessingGame secret

{-
  WORKED EXAMPLE 2:

  Exercise: Write a function createTempsFile that asks the user for the temperature of 5
  cities listed below. Assume that the temperatures are non-negative integers. The function
  should then produce a horizontal bar chart in the form of a text file. The text file, titled
  temps.txt, should show the name of the city and a star symbol (*) for every degree Celsius.
  Here are the cities (add them to Week8.hs):

  testCities = ["London", "Paris", "New York", "Tokyo", "Sydney"]

  Below is an example of what temps.txt might contain after the program has been executed:

  London ********
  Paris ******
  New York ************
  Tokyo ********************
  Sydney *******************
-}

testCities = ["London", "Paris", "New York", "Tokyo", "Sydney"]

addToTempsFile :: [String] -> IO ()
addToTempsFile [] = return ()
addToTempsFile (city : cities) = do
  putStr ("Enter the temperature in " ++ city ++ ": ")
  temp <- getLine
  let tempInt = read temp :: Int
  appendFile "temps.txt" city
  appendFile "temps.txt" (replicate (10 - length city) ' ')
  appendFile "temps.txt" (replicate tempInt '*' ++ "\n")
  addToTempsFile cities

-- Works as intended, simply delete 'temps.txt' and run 'createTempsFile' to see :)"
createTempsFile :: IO ()
createTempsFile = do
  writeFile "temps.txt" ""
  addToTempsFile testCities

{-
  EXERCISES: 

  1. Write a greeting program that asks the user for their name, and then outputs a personalised greeting. 
  For example, on input “Sam”, the program displays “Hello, Sam”.
-}

greeting :: IO ()
greeting = do
  putStrLn "What is your name?: "
  name <- getLine
  putStrLn ("Hello, " ++ name ++ ".")


{-
  2. Write an addTwoNumbers program that reads two integers from the user, each on a
  separate line, and displays their sum.
-}
addTwoNumbers :: IO ()
addTwoNumbers = do
  putStrLn "Enter your first number to be added: "
  num1 <- getLine
  putStrLn "Enter your second number to be added: "
  num2 <- getLine
  let a = read num1 :: Int
  let b = read num2 :: Int
  let c = a + b
  putStrLn (show a ++ "+" ++ show b ++ "=" ++ show c)


{-
  3. Write a copyFile program that copies a text-file. The program should ask the user
  for the name of the file to copy (including the extension, for example “test.txt”). The
  program should also ask the user for the name of the copy (e.g. “copy.txt”).
-}
copyFile :: IO ()
copyFile = do
  putStrLn "What is your file's name? Please include the extension (e.g. 'file.txt'): "
  fileName <- getLine
  putStrLn "What do you want the name of this copy to be?: "
  copyName <- getLine
  contents <- readFile fileName
  writeFile copyName contents

{-
  4. Write a function that allows the user to build up a list of strings by entering them in
  one at a time, and displays the list after every step. The program should exit when the
  user enters an empty string (no need to return the list of strings). For example:

  Enter a line: hello
  List is now ["hello"]
  Enter a line: world
  List is now ["hello","world"]
  Enter a line:

  Hint: use two functions for this question. First write a recursive function buildList
  with the following signature that that builds a list step-by-step:

  buildList :: [String] -> IO ()

  Next write another listBuilder function that starts off the computation by calling
  buildList with a suitable initial parameter value:

  listBuilder :: IO ()
-}

buildList :: [String] -> IO ()
buildList strings = do
  putStrLn "Enter a line: "
  str <- getLine 
  if str == ""
    then return ()
  else do 
    let newList = strings ++ [str]
    putStrLn ("The list is now: " ++ show newList)
    buildList newList

listBuilder :: IO ()
listBuilder = do
  buildList []

{-
  5. Write a function that reads an integer n from the user then reads n integers (on separate lines), 
  and finally displays the sum of these n numbers read.
-}
displaySum :: [Int] -> IO ()
displaySum [] = putStrLn "No Numbers were passed."
displaySum numbers = do
  let total = sum numbers
  putStrLn ("The sum of " ++ show numbers ++ " = " ++ show total)

numListBuilder :: [Int] -> IO ()
numListBuilder numbers = do
  putStrLn "Enter a number to be added (= to sum them): "
  x <- getLine
  if x == "="
    then displaySum numbers
  else do 
    let num = read x :: Int
    let newNumbers = numbers ++ [num]
    putStrLn ("Numbers to be added: " ++ show newNumbers)
    numListBuilder newNumbers

{-
  6. In this question, we will guide you to write a program that maintains a list of words in
  a file. The program should also allow the user to add words, display all the words in
  the file, and display all the words of a specified length. We have split the code for this
  program does into two parts: functional code and user interface code.

  FUNCTIONAL CODE: 

  (a) Begin by writing a function addWord that adds a string to the end of a list.
  Here is the signature of addWord:

  addWord :: String -> [String] -> [String]
-}
addWord :: String -> [String] -> [String]
addWord "" _ = []
addWord str list = list ++ [str]

-- Tested with ```addWord "lemon" ["apple","banana"]```
-- Output: ["apple","banana", "lemon"]

{-
  (b) Write a function that turns a list of strings into a multiline string (the function
  should return a single string where each string in the given list is separated with
  the new line character "\n").

  wordsToString :: [String] -> String
-}
wordsToString :: [String] -> String
wordsToString strings = concat (map (++ "\n") strings)

-- Tested with ```wordsToString ["apple","banana","lemon"]```
-- Output: "apple\nbanana\nlemon"

{-
  (c) Write a function that, given a list and a length, returns a list of all words of the given length:
  wordsOfLength :: Int -> [String] -> [String]
-}

wordsOfLength :: Int -> [String] -> [String]
wordsOfLength wordLength = filter (\word -> length word == wordLength)

-- Tested with ```wordsOfLength 5 ["apple","banana","lemon"]```
-- Output: ["apple", "lemon"]

{-
  UUSER INTERFACE CODE: 

  Begin by creating a text file named “words.txt”. Then copy the following line into this
  file (this is an initial list of words for your program):

  ["apple"]
  
  Now, write a program (i.e. a main function) which performs the following steps in
  order:
  
  (a) Using readFile read the contents of words.txt (as a single string). Next, using
  read turn this string into a list of strings (see worked example 1).

  (b) Using addWord, add the word “lemon” to the list.

  (c) Using wordsToString and putStrLn, display the words on the screen.

  (d) Using show and writeFile, turn the list of strings into a single string and write it
  back to words.txt.
-}
{- ORIGINAL CODE:
main :: IO ()
main = do
  contents <- readFile "words.txt"

  let list = read contents :: [String]
  let newList = addWord "lemon" list

  putStrLn (wordsToString newList)

  let string = show newList

  writeFile "words.txt" string
-}

{-
  Modify your program so that it performs the first step (reading the list in from the file)
  when it starts, and then provides a menu to the user with the following options:

  (a) add a word to the list
  (b) display all words
  (c) display all words of a given length
  (d) exit

  On exiting the program, the list should be written back to the file.
-}
funcA :: [String] -> IO [String]
funcA currentList = do
  putStrLn "Enter a word to be added: "
  word <- getLine

  let newList = addWord word currentList

  putStrLn (word ++ " has been added to the list.")

  return newList

funcB :: [String] -> IO ()
funcB currentList = do
  
  putStrLn (wordsToString currentList)

funcC :: [String] -> IO ()
funcC currentList = do

  putStrLn "Enter an integer to find the items matching that length: "
  wordLength <- getLine 
  let num = read wordLength :: Int

  putStrLn (wordsToString (wordsOfLength num currentList))

funcD :: [String] -> IO ()
funcD currentList = do
  writeFile "words.txt" (show currentList)
  putStrLn "Exiting program... goodbye!"

loop :: [String] -> IO ()
loop list = do
  putStr "(a): add a word to the list\n(b): display all words\n(c): display all words of a given length\n(d): exit\n"
  putStrLn "Please enter your input: "
  input <- getLine

  case input of 
    "a" -> do
      newList <- funcA list 
      loop newList
    "b" -> do
      funcB list
      loop list
    "c" -> do
      funcC list 
      loop list
    "d" -> funcD list 
    _ -> do 
      putStrLn "Invalid input, try again."
      loop list
  
main :: IO ()
main = do
  contents <- readFile "words.txt" 
  let list = read contents :: [String]
  loop list

  




