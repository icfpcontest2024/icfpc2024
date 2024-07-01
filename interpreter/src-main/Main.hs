{-# LANGUAGE LambdaCase #-}
module Main where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Parser ( pTerm )
import ICFPC2024.Printer ( ppTerm )
import ICFPC2024.Interpreter ( interpret )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import System.Environment

main :: IO ()
main = getArgs >>= \case
  ["encode"] -> encode
  ["eval"] -> eval
  _ -> putStrLn "Usage: icfpc2024-interpreter (encode | eval)"

eval :: IO ()
eval = do
  mbT <- pTerm . BSL8.fromStrict <$> BS8.getLine
  case mbT of
    Left e -> putStrLn $ "Parsing failure: " ++ show e
    Right t -> do
      print t
      case interpret False t of
        Left e -> print e
        Right (r, steps) -> do 
          putStrLn $ show steps ++ " beta reductions"
          case r of
            TString s -> BSL8.putStrLn s
            _ -> print r

encode :: IO ()
encode = do
  str <- BSL8.getContents
  BSL8.putStrLn $ ppTerm $ TString str
