module Compression.Pi where
import Data.Char (ord, chr);
import Data.List (isSubsequenceOf);
import Data.Maybe (fromJust, isNothing);
import Data.Number.CReal (showCReal);

type Digit = Int;

-- | @compress k@ equals the 2-tuple (a,b) such that
-- @take b (drop a $ digitsOfPi $ b + a) == k@.
compress :: Integer -> (Integer, Integer);
compress = recurse 1
  where
  recurse :: Integer -> Integer -> (Integer, Integer)
  recurse n desired
    | isNothing position = recurse (n * 10) desired
    |otherwise = (fromJust position, toEnum $ length $ digits desired)
    where
    position :: Maybe Integer
    position = subPosition (digits desired) (digitsOfPi n)

-- | @subPosition a b@ equals the value k such that
-- @take (length a) $ drop (fromJust k) b@ equals a if a is a
-- subsequence of b.
-- @subPosition a b@ otherwise equals Nothing.
subPosition :: Eq a => [a] -> [a] -> Maybe Integer
subPosition a b = recurse a b 0
  where
  recurse :: Eq a => [a] -> [a] -> Integer -> Maybe Integer
  recurse a b c
    | not $ a `isSubsequenceOf` b = Nothing
    | take (length a) b == a = Just c
    | otherwise = recurse a (tail b) (c + 1);

-- | @digitsOfPi k@ equals the list of the first k digits of pi.
digitsOfPi :: Integer -> [Digit];
digitsOfPi n = init $ digits $ read pee
  where pee = filter (/='.') $ showCReal (fromEnum n) pi;

-- | @digits k@ equals the list of the digits of k.
digits :: Integer -> [Digit];
digits = map (read . (:[])) . show;

-- | @stringToInteger k@ equals an 'Integer' value which uniquely
-- represents k.
--
-- stringToInteger is the inverse function of 'integerToString'.
stringToInteger :: String -> Integer;
stringToInteger = recurse 0
  where
  recurse :: Integer -> String -> Integer
  recurse a c
    | c == [] = a
    | otherwise = recurse newA (tail c)
    where
    newA :: Integer
    newA = a * 256 + (firstBit c)
    firstBit :: String -> Integer
    firstBit = toEnum . fromEnum . (!!0);

-- | @integerToString k@ equals an Integer value which uniquely
-- identifies k.
--
-- integerToString is the inverse function of 'stringToInteger'.
integerToString:: Integer -> String
integerToString = recurse []
  where
  recurse :: String -> Integer -> String
  recurse a c
    | c <= 0 =  a
    | otherwise = recurse newA newC
    where
    newA :: String
    newA = (toEnum $ fromInteger remainder) : a
    newC :: Integer
    newC = div (c - remainder) 256
    remainder :: Integer
    remainder = c `rem` 256;
