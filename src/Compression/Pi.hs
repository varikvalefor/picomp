module Compression.Pi where
import Data.Char (ord, chr);
import Data.List (isSubsequenceOf);
import Data.Maybe (fromJust, isNothing);
import Data.Number.CReal (showCReal);

-- | For all 'Digit' @k@, @k@ represents a decimal digit.
type Digit = Int;

-- | @compress k@ equals the 2-tuple @(a,b)@ such that
-- @take b (drop a $ digitsOfPi $ b + a) == k@.
compress :: Integer -> (Integer, Integer);
compress = recurse 1
  where
  recurse :: Integer -> Integer -> (Integer, Integer)
  recurse n query
    | isNothing position = recurse (n * 10) query
    |otherwise = (fromJust position, toEnum $ length $ digits query)
    where
    position :: Maybe Integer
    position = subPosition (digits query) $ digitsOfPi n

-- | @decompress (a,b)@ equals the Integer @g@ such that the digits of
-- @g@ begin at the @a@th digit of pi.
decompress :: (Integer, Integer) -> Integer;
decompress (len, pos) = listToInt $ drop pos' $ digitsOfPi $ len + pos
  where
  pos' :: Int
  pos' = fromEnum pos
  --
  listToInt :: [Digit] -> Integer
  listToInt = read . concat . map show;

-- | @subPosition a b@ equals the value $k$ such that
-- @take (length a) $ drop (fromJust k) b@ equals $a$ if $a$ is a
-- subsequence of $b$, otherwise equalling Nothing.
subPosition :: Eq a => [a] -> [a] -> Maybe Integer
subPosition a b = recurse a b 0
  where
  recurse :: Eq a => [a] -> [a] -> Integer -> Maybe Integer
  recurse a b c
    | not $ a `isSubsequenceOf` b = Nothing
    | take (length a) b == a = Just c
    | otherwise = recurse a (tail b) (c + 1);

-- | @digitsOfPi k@ equals the list of the first @k@ digits of pi.
digitsOfPi :: Integer -> [Digit];
digitsOfPi n = init $ digits $ read pee
  where pee = filter (/='.') $ showCReal (fromEnum n) pi;

-- | @digits k@ equals the list of the digits of @k@.
digits :: Integer -> [Digit];
digits = map (read . (:[])) . show;

-- | @stringToInteger k@ equals an 'Integer' value which uniquely
-- represents @k@.
--
-- @stringToInteger@ is the inverse function of @'integerToString'@.
stringToInteger :: String -> Integer;
stringToInteger = recurse 0
  where
  recurse :: Integer -> String -> Integer
  recurse a c
    | c == [] = a
    | otherwise = recurse (a * 128 + firstBit c) (tail c)
    where
    firstBit :: String -> Integer
    firstBit = toEnum . fromEnum . (!!0);

-- | @integerToString k@ equals the 'String' value which @k@ uniquely
-- represents.
--
-- @integerToString@ is the inverse function of @'stringToInteger'@.
integerToString:: Integer -> String
integerToString = recurse []
  where
  recurse :: String -> Integer -> String
  recurse a c
    | c <= 0 =  a
    | otherwise = recurse a' c'
    where
    a' :: String
    a' = toEnum (fromInteger remainder) : a
    c' :: Integer
    c' = (c - remainder) `div` 128
    remainder :: Integer
    remainder = c `rem` 128;
