doubleMe x = x + x

factorial :: Int -> Int
factorial n = product [1 .. n]

-- List comprehension syntax
--    [f(x) | x <- list, pred] eg [ x + 1 | x <- list, x < 10]
--    f(x) = mapping function / output function
--    x <- list = set annotation
--    pred = predicate, condition for a value in the list to be picked
--

-- Syntax in Functions
-- Pattern matching
--  this consist of specific patterns to which some data should conform and then
--  checking to see if it does and deconstrucing the data according to the pattern (Deconstruction)
--
--  When definig a function, seperate function  bodies can be defined for different patterns (Parameter patterns)
--  Uses of pattern matching
--    1. to define different function body based on a pattern
--    2. deconstruction of a container, eg Tuple
--    3. use in list comprehension
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN"
lucky x = "Sorry, you are out of luck"

-- when the function is called the pattern is matched from top to bottom

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Not between 1 and 5"

-- for the factorial function we can also use the pattern matching approach, in a recursive format
--
factorialPat :: (Integral a) => a -> a
factorialPat 0 = 1
factorialPat x = x * factorialPat (x - 1)

-- Non exhaustive matching (this would throw an error when an unmatched patterns is called)
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

-- Pattern matching can also be used on tuples. the tuple is then deconstructed
addTuple :: (Num a) => (a, a) -> (a, a) -> (a, a)
addTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Lists can also be used in pattern matching
--  list can be matched with [] or any pattern that involves `:`
--  E.g.
--
getHead :: [a] -> a
getHead [] = error "Can't get head from an empty list"
getHead (x : _) = x

-- When binding several variable we have to suround this by a parenthesis
--
compFirstAndSecond :: (Ord a) => [a] -> Bool
compFirstAndSecond [] = error "List doesn't contain enough content"
compFirstAndSecond [x] = error "List doesn't contain enough content"
compFirstAndSecond (first : second : _) = first < second

-- Get last element with list pattern matching
getLast :: [a] -> a
getLast [] = error "empty list"
getLast [last] = last
getLast (_ : xs) = getLast xs

-- ## Patterns: a way to break something up according to a pattern and then bind it to a name whitst keeping a reference to the whole thing
-- done by putting an `@` in front of a pattern.
--  e.g. xs@(x:y:ys)
--    This is the same as matching `x:y:ys` on a list, in the body of a function the list xs can be referenced
capital :: String -> String
capital "" = "Empty string"
capital all@(c : sub_string) = "The first letter of " ++ all ++ " is " ++ [c]

-- ## Guards
-- While Patterns are ways to make sure a value conforms to a form and to also deconstruct the value, guards are used to test whether some property
-- of the value (or several) is true
--    Patterns == check struct
--    Guards == validate property of the value, check if expression using the input evaluates to `True`
-- Example below

densityTell :: (RealFloat a) => a -> String
densityTell density
  | density < 1.2 = "Wow! You're going for a ride in the sky!"
  | density <= 1000.0 = "Have fun swimming, but watch out for sharks!"
  | otherwise = "if it's sink or swim, you're going to sink."

-- Gaurds are indicated by pipes that follow a function's name and it's parameter, they are basically boolean expressions. that when evaluated to `True` the corresponding function body is used
-- the guards can be written inline
--
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- ## Where in Guards
--
-- This is used to define a variable use in Guard expression
-- Eg
densityTellSample :: (RealFloat a) => a -> a -> String
densityTellSample mass volume
  | density < air = "Wow! You're going for a ride in the sky!"
  | density <= water = "Have fun swimming, but watch out for sharks!"
  | otherwise = "if it's sink or swim, you're going to sink."
  where
    density = mass / volume
    air = 1.2
    water = 1000.0

-- Where bindings aren't shared across function bodies of different patterns. to allow several patterns of a function to access the same
-- where binding it has to be defined globally
-- where binding can also be used to pattern match
-- The above example can where section of the example above can be written as
-- ````
--   where density = mass / volume
--         (air, water) = (1.2, 1000.0)

-- Example of pattern match in where
initials :: String -> String -> String
initials firstName lastName = "Initials : " ++ [f] ++ "." ++ [l]
  where
    (f : _) = firstName
    (l : _) = lastName

-- functions can also be defined in the where block of a function definition
-- example
--
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
  where
    density mass volume = mass / volume

-- ## Let
-- syntax: let <bindings> in <expression>
-- This is similar to where bindings. where bindings are syntactic constuct that let you binf to variable at the end of a function and the variable is visible
-- to the whole function including the guards. let binding allows you to bind variable anywhere and it is very local to the point where it was defined
-- can also use pattern matching to declare bindings
-- let is an expression, so it can be put into any other expression
-- let can also be used to introduce/define functions
-- binding several variables inline can be done using `;` to seperate them
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

-- sample in-line declaration on left
-- (let a = 100; b = 200 in a*b, let foo="Hey"; bar = "there!" in foo ++ bar)
--
-- let can also be used in list comprehension at the same point as a predicate just that this doesn't filter
-- multiple predicates and let expression can come after the set annotation
calcDensities' :: (RealFloat a) => [(a, a)] -> [a]
calcDensities' xs = [density | (m, v) <- xs, let density = m / v]

--
--
-- ## Case expressions
-- These are expressions like if else expressions and let bindings. not only can expressions be evaluated based on the possible
-- cases of the value.  pattern matching can also be done.
-- syntax:
-- case <expression> of <pattern> -> result
--                      <pattern> -> result
--                      <pattern> -> result
--                      <pattern> -> result
--                      ...
--
-- pattern matching on parameters of a function are syntactic sugar of case expressions
-- So
--    head' :: [a] -> a
--    head' [] = error "Error"
--    head' (x:_) = x
-- Is the same as
--    head' :: [a] -> a
--    head' xs = case xs of [] -> error "Error"
--                          (x:_) -> x
--
--
-- cases are also evaluated from top to bottom, if the variable doesn't match any of the case and exception is thrown
-- while pattern matching can only be used in function definition, case can be used pretty much anywhere
--
-- ### Recursion
-- This is a way to define functions in which the function is applied inside it own definition.
-- Functions definitions in Mathematics are often defined in a recursive way
-- When defining a recursive function you have to ensure to define the `edge` case to prevent infinite recursion which would throw an error.
-- In Haskell recursion is important because the language is a declarative language where we declare what something is instead of how it is done.
-- The language doesn't support loops instead this behaviour is achieved through recursion
--
-- An example of loop implementation in a imperative language done with recursion in Haskell
-- maximum - returning the maximum element in a list
--
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximu of an empty list is null"
maximum' [x] = x
maximum' (x : xs)
  | x < maximumTail = maximumTail
  | otherwise = x
  where
    maximumTail = maximum' xs

-- E.g 2 replication - take a value and a size and return a list with the value
replicate' :: (Num s, Ord s) => s -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

--
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- Implementing quicksort using recursion
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [sm | sm <- xs, sm < x]
      biggerSorted = quicksort [bg | bg <- xs, bg >= x]
   in smallerSorted ++ [x] ++ biggerSorted

-- Often edge cases in recursive function turns out to be identity.
-- e.g. edge case of sum of element is 0, because 0 is an identity for addition `0 + x = x`
-- for product of elements it is 1,  1 * x = x
--
--
-- ## Higher Order Functions f(g(x))
-- In Haskell functions can take functions as a parameter and also return a function as a value
--

-- ** Curried functions

-- Every function in Haskell officially only take one parameter, so how are functions defined to take more than one parameter.
-- There is a clever trick to this **Curried functions** all functions accepting more than one parameter so far has been curried functions
-- Curried functions
--  - e.g for Max function which compares two parameters A and B. This can be thought of as first there is a function that take any parameter x and
--  return A or that parameter y depending on which is bigger, lets call that function g(x) then we that function g(x) takes x as B. To define the function
--  g(x) we need a function that takes A and return a function g(x) which would be f(y)
--     f(y) -> g(x) where g(x) = Max(x,y)
--  hence the syntax x -> y -> out = x -> (y -> out).
-- For multi parameter functions calling the function with partial parameters would return a function that accepts the rest of the parameter and applies the initial parameters
--
-- Infix functions can also be partially applied by using sections. To section an infix functions you simply surround it would parentheses and only supply a parameter on one side
-- Definition of currying
-- Currying is a computer science and mathematics technique that transforms a function taking multiple arguments into a sequence of nested functions, each taking exactly one argument
--
-- As stated above a function can take a function as a parameter
-- in other to define the function type as an input parameter the function type definition has to be in parentheses `Mandatory`
--
-- E.g
applyTwice :: (a -> a) -> a -> a
applyTwice func v = func (func v)

--
-- function that flips the order of the input parameter of a function
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- ## HOF - Mapping and filters
--
-- maps takes a list and applies a function on it then returns a new list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

-- filter takes a list and  applies a function on it then return a subset of the list that has the function return true
--
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs

--
-- Implementing Collatz sequence
--
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

--
-- Lambdas
-- This are basically anonymous functions that are used because we need some functions only once. Lambda functions are normally created to be passed to high order functions
-- lambda functions are prefixed with `\` in Haskell
-- e.g passing a function to map
-- multiplyByTwo :: (Num a) => [a] -> [a]
-- multiplyByTwo xs = map (\x -> x * 2) xs

-- can be reduced to
-- multiplyByTwo = map (\x -> x * 2)
--
-- or more idiomatic approach is to just use partial function
-- multiplyByTwo = map (*2)
--
-- pattern matching can also be used in lambda functions
--
-- ## Folds
-- This are functions that take a binary function, a staring value and a list and returns a single value by folding the list
-- The binary function is called an accumulator, e.g foldl, foldr,
-- e.g. implementing sum with fold

-- sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc x -> acc + x) 0 xs
-- sum' = foldl (\acc x -> acc + x) 0
-- sum' = foldl (+) 0
--
-- sumFromRight :: (Num a) => [a] -> a
-- sumFromRight xs = foldr (\x acc -> x + acc) 0 xs

mapFold :: (a -> b) -> [a] -> [b]
mapFold f xs = foldr (\x acc -> f x : acc) [] xs
