import Compression.Pi;
import System.Environment;

main :: IO ();
main = getArgs >>= deicide

deicide :: [String] -> IO ()
deicide a
  | a !! 0 == "compress" = print $ compress $ stringToInteger $ a !! 1
  | otherwise = error "An invalid command is entered."
