{-# LANGUAGE DeriveGeneric #-}
module ICFPC2024.AST where

import GHC.Generics
import Data.List
import Data.ByteString.Lazy.Char8 ( ByteString )

data Term
  = TInt Integer
  | TString ByteString
  | TBool Bool
  | TVar Integer
  | TLam Integer Term
  | TUnOp Char Term
  | TBinOp Term Char Term
  | TIf Term Term Term
  deriving ( Generic, Eq, Ord )

tapp :: Term -> Term -> Term
tapp = flip TBinOp '$'

instance Show Term where
  show (TInt i) = show i
  show (TString s) = show s
  show (TBool b) = show b
  show (TVar v) = 'v' : show v
  show (TLam v t) = "(v" ++ show v ++ " -> " ++ show t ++ ")"
  show (TUnOp c t) = "(" ++ [c] ++ " " ++ show t ++ ")"
  show (TBinOp l c r) = "(" ++ show l ++ " " ++ [c] ++ " " ++ show r ++ ")"
  show (TIf b t f) = "(if " ++ show b ++ " then " ++ show t ++ " else " ++ show f ++ ")"

chars :: [Char]
chars = ['!'..'~']

-- Including space and newline, but excluding { and }
charsDecoded :: [Char]
charsDecoded = nub $
  ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ (chars \\ "{}") ++ " \n"

-- Y combinator, for recursion
yComb :: Term
yComb = TLam 1 (t `tapp` t) where
  t = TLam 2 (TVar 1 `tapp` (TVar 2 `tapp` TVar 2))
