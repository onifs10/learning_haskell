doubleMe x = x + x

factorial :: Int -> Int
factorial n = product [1..n]


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
factorialPat x = x * factorialPat (x-1)

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
--  Eg
--
getHead  :: [a] -> a
getHead [] = error "Can't get head from an empty list"
getHead (x:_) = x

-- When binding several variable we have to suround this by a parenthesis
--
compFirstAndSecond :: (Ord a) => [a] -> Bool
compFirstAndSecond [] = error "List doesn't contain enough content"
compFirstAndSecond [x] = error "List doesn't contain enough content"
compFirstAndSecond (first:second:_) = first < second

-- Get last element with list pattern matching
getLast :: [a] -> a
getLast [] = error "empty list"
getLast [last] = last
getLast (_:xs) = getLast xs

-- ## Patterns: a way to break something up according to a pattern and then bind it to a name whitst keeping a reference to the whole thing
-- done by putting an `@` in front of a pattern. 
--  e.g. xs@(x:y:ys)
--    This is the same as matching `x:y:ys` on a list, in the body of a function the list xs can be referenced
capital :: String -> String
capital "" = "Empty string"
capital all@(c:sub_string) = "The first letter of " ++ all ++ " is " ++ [c]

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
  where density = mass / volume
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
  where (f:_) = firstName
        (l:_) = lastName

-- functions can also be defined in the where block of a function definition
-- example
--
calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs = [density m v | (m, v) <- xs]
  where density mass volume = mass / volume

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
      topArea = pi * r^2
  in sideArea + 2 * topArea

-- sample inline declarion on left
-- (let a = 100; b = 200 in a*b, let foo="Hey"; bar = "there!" in foo ++ bar)
--
-- let can also be used in list comprehension at the same point as a predicate just that this doesn't filter
-- multiple predicates and let expression can come after the set anotation
calcDensities' :: (RealFloat a) => [(a, a)] -> [a]
calcDensities' xs = [density | (m,v) <- xs, let density = m / v]
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
-- pattern matching on parameters of a function are synctactic sugar of case expressions
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
