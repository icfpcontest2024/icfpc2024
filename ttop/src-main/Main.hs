module Main where

import TTop
import TTop.Board
import TTop.History
import Data.Functor
import Control.Monad
import Options.Applicative
import qualified Data.List as List
import qualified Data.ByteString.Char8 as BS8

data Config = Config
  { printStates :: Bool
  , disableOpt :: Bool
  , fname :: String
  , inputs :: [Integer]
  }
  deriving stock Show

configParser :: Parser Config
configParser = Config
  <$> switch (
        long "print-states"
      )
  <*> switch (
        long "disable-opt"
        <> help "disable optimisations"
      )
  <*> strArgument (
        metavar "BOARD.brd"
      )
  <*> many (argument auto (
        metavar "INPUT"
      ))

configInfo :: ParserInfo Config
configInfo = info (configParser <**> helper)
  ( fullDesc
  <> progDesc "Run the world"
  <> header "Time-travel-oriented programming"
  )

main :: IO ()
main = do
  config <- execParser configInfo <&> \cfg ->
    case cfg.inputs of
      [] -> cfg{ inputs = [6, 7] }
      _ -> cfg

  let format
        | ".csv" `List.isSuffixOf` config.fname = formatCsv
        | otherwise = formatText

  (parseBoard format <$> BS8.readFile config.fname) >>= \case
    Left e -> BS8.putStrLn e
    Right board -> do
      let (sts, outcome) = runProgram config.disableOpt Nothing $ newState (map I config.inputs) board
      when config.printStates $
        forM_ sts $
          BS8.putStrLn . renderState
      mapM_ BS8.putStrLn $ renderOutcome outcome
