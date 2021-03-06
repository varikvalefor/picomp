import Compression.Pi;
import System.Environment;

main :: IO ();
main = getArgs >>= deicide
  where
  deicide :: [String] -> IO ()
  deicide a = case head a of
    "compress"   -> print $ compress $ stringToInteger $ a !! 1
    "decompress" -> putStrLn $ integerToString $
                    decompress (read $ a !! 1, read $ a !! 2)
    _            -> error "An invalid command is entered.";
