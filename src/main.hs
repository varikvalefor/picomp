import Compression.Pi;
import System.Environment;

main :: IO ();
main = getArgs >>= deicide

deicide :: [String] -> IO ()
deicide a
  | a !! 0 == "compress" = print $ compress $ stringToInteger $ a !! 1
  | a !! 0 == "decompress" = putStrLn $ integerToString $
    decompress (read $ a !! 1, read $ a !! 2)
  | otherwise = error "An invalid command is entered."
