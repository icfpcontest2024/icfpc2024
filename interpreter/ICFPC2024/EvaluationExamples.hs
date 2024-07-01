module ICFPC2024.EvaluationExamples where

import ICFPC2024.AST
import ICFPC2024.Interpreter
import ICFPC2024.Printer
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Either

helloWorld :: Term
helloWorld = TLam 2 (TLam 3 (TVar 2)) `tapp` TBinOp (TString "Hello") '.' (TString " World!") `tapp` TInt 42

unaryOps :: [Term]
unaryOps =
  [ TUnOp '-' $ TInt 3
  , TUnOp '!' $ TBool True
  , TUnOp '#' $ TString "test"
  , TUnOp '$' $ TInt 15818151
  ]

binaryOps :: [Term]
binaryOps =
  [ TBinOp (TInt 2) '+' (TInt 3)
  , TBinOp (TInt 3) '-' (TInt 2)
  , TBinOp (TInt 3) '*' (TInt 2)
  , TBinOp (TInt (-7)) '/' (TInt 2)
  , TBinOp (TInt (-7)) '%' (TInt 2)
  , TBinOp (TInt 3) '<' (TInt 2)
  , TBinOp (TInt 3) '>' (TInt 2)
  , TBinOp (TInt 3) '=' (TInt 2)
  , TBinOp (TBool True) '|' (TBool False)
  , TBinOp (TBool True) '&' (TBool False)
  , TBinOp (TString "te") '.' (TString "st")
  , TBinOp (TInt 3) 'T' (TString "test")
  , TBinOp (TInt 3) 'D' (TString "test")
  ]

ifExample :: Term
ifExample = TIf (TBinOp (TInt 2) '>' (TInt 3)) (TString "yes") (TString "no")

evalExample :: [Term]
evalExample =
  [ TLam 2 ((TLam 1 (TBinOp (TVar 1) '+' (TVar 1))) `tapp` (TBinOp (TInt 3) '*' (TInt 2))) `tapp` (TVar 23)
  , (TLam 1 (TBinOp (TVar 1) '+' (TVar 1))) `tapp` (TBinOp (TInt 3) '*' (TInt 2))
  , TBinOp (TBinOp (TInt 3) '*' (TInt 2)) '+' (TBinOp (TInt 3) '*' (TInt 2))
  , TBinOp (TInt 6) '+' (TBinOp (TInt 3) '*' (TInt 2))
  , TBinOp (TInt 6) '+' (TInt 6)
  , TInt 12
  ]

stepCounting :: Term
stepCounting = yComb `tapp`
  TLam 1 (TLam 2 $
          TIf (TBinOp (TVar 2) '=' (TInt 0))
          (TInt 1)
          (TLam 3 (TBinOp (TVar 1 `tapp` TVar 3) '+' (TVar 1 `tapp` TVar 3))
           `tapp` TBinOp (TVar 2) '-' (TInt 1))
         ) `tapp` TInt 4


teq :: Term -> Term -> Term
teq l = TBinOp l '='

strconc :: Term -> Term -> Term
strconc l = TBinOp l '.'

-- | A term that uses all constructs, to check interpreters
selfCheck :: Term
selfCheck = foldl (\body (nm, ch) -> TIf ch body (TString $ nm <> " is not correct")) answer ts where
  -- The answer you get when it is correct
  answer = TString "Self-check OK, send `solve language_test " `strconc` TUnOp '$' (TBinOp (TInt 2) '+' $ TBinOp (TInt 311) '*' (TInt 124753942619)) `strconc` TString "` to claim points for it"
  -- Terms that should all evaluate to 'True'
  ts =
    [ ("if", TBool True)
    , ("binary =", TInt 3 `teq` TInt 3)
    , ("binary =", TBool False `teq` TBool False)
    , ("binary =", TString "test" `teq` TString "test")
    , ("binary -", TInt 3 `teq` TBinOp (TInt 5) '-' (TInt 2))
    , ("unary -", TUnOp '-' (TInt 3) `teq` TBinOp (TInt 2) '-' (TInt 5))
    , ("unary !", TUnOp '!' (TBool False))
    , ("unary #", TUnOp '#' (TString "test") `teq` TInt 15818151)
    , ("unary $", TUnOp '$' (TInt 15818151) `teq` TString "test")
    , ("binary +", TInt 3 `teq` TBinOp (TInt 1) '+' (TInt 2))
    , ("binary *", TInt 6 `teq` TBinOp (TInt 2) '*' (TInt 3))
    , ("binary /", TInt 2 `teq` TBinOp (TInt 7) '/' (TInt 3))
    , ("binary /", TInt (-1) `teq` TBinOp (TInt (-3)) '/' (TInt 2))
    , ("binary %", TInt 1 `teq` TBinOp (TInt 7) '%' (TInt 3))
    , ("binary %", TInt (-1) `teq` TBinOp (TInt (-3)) '%' (TInt 2))
    , ("binary >", TBinOp (TInt 3) '>' (TInt 2))
    , ("binary <", TBinOp (TInt (-3)) '<' (TInt (-2)))
    , ("binary |", TBinOp (TBool False) '|' (TBool True))
    , ("binary |", TUnOp '!' $ TBinOp (TBool False) '|' (TBool False))
    , ("binary &", TBinOp (TBool True) '&' (TBool True))
    , ("binary &", TUnOp '!' $ TBinOp (TBool True) '&' (TBool False))
    , ("binary .", TBinOp (TString "te") '.' (TString "st") `teq` TString "test")
    , ("binary T", TBinOp (TInt 3) 'T' (TString "test") `teq` TString "tes")
    , ("binary D", TBinOp (TInt 3) 'D' (TString "test") `teq` TString "t")
    , ("application", (TLam 3 (TVar 3) `tapp` TInt 10) `teq` TInt 10)
    , ("application", (TLam 3 (TLam 3 $ TLam 3 $ TLam 2 $ TVar 3) `tapp` TInt 1 `tapp` TInt 2 `tapp` TInt 3 `tapp` TInt 4) `teq` TInt 3)
   ]

printExamples :: IO ()
printExamples = do
  BS8.putStrLn $ "get index"
  BS8.putStrLn $ ppTerm $ TString "get index"
  BS8.putStrLn "Hello World!"
  BS8.putStrLn $ ppTerm $ TString "Hello World!"
  BS8.putStrLn "Unary ops"
  forM_ unaryOps $ \unTerm -> do
    print unTerm
    BS8.putStrLn $ ppTerm unTerm <> " -> " <> BS8.pack (show $ fst $ fromRight (error "Expected right") $ interpret True unTerm)
  BS8.putStrLn "Binary ops"
  forM_ binaryOps $ \unTerm -> do
    print unTerm
    BS8.putStrLn $ ppTerm unTerm <> " -> " <> BS8.pack (show $ fst $ fromRight (error "Expected right") $ interpret True unTerm)
  BS8.putStrLn "If"
  print ifExample
  BS8.putStrLn $ ppTerm ifExample
  print helloWorld
  BS8.putStrLn $ ppTerm helloWorld
  BS8.putStrLn "Eval example"
  forM_ evalExample print
  forM_ evalExample $ BS8.putStrLn . ppTerm
  BS8.putStrLn "Step counting"
  print stepCounting
  BS8.putStrLn $ ppTerm stepCounting
  print $ interpret True stepCounting
  BS8.putStrLn "Self check"
  print selfCheck
  BS8.putStrLn $ ppTerm selfCheck
  print $ interpret True selfCheck
