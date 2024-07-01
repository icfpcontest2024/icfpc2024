module ICFPC2024.Printer (
  ppTerm,
  toBase94,
  encodeString,
  generateEncodeString,
) where

import ICFPC2024.AST

import Data.Maybe ( fromMaybe )
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8
import Control.Monad ( forM_ )

-- | Pretty print a term to the ICFP string representation
ppTerm :: Term -> ByteString
ppTerm = toLazyByteString . f where
  f :: Term -> Builder
  f (TInt i)
    | Just b94 <- toBase94 i = "I" <> b94
    | otherwise = f $ TUnOp '-' (TInt (-i))
  f (TString s) = "S" <> encodeString s
  f (TBool True) = "T"
  f (TBool False) = "F"
  f (TVar v) = "v" <> fromMaybe (error "Negative variable found") (toBase94 v)
  f (TLam i t) = "L" <> fromMaybe (error "Negative variable found") (toBase94 i) <> " " <> f t
  f (TUnOp c t) = "U" <> char7 c <> " " <> f t
  f (TBinOp l c r) = "B"  <> char7 c <> " "  <> f l <> " " <> f r
  f (TIf b tr fl) = "? " <> f b <> " " <> f tr <> " " <> f fl

-- | Convert an integer to its base94 representation,
--   efficiently using a bytestring builder and tail recursion
toBase94 :: Integer -> Maybe Builder
toBase94 0 = Just $ word8 33
toBase94 x | x < 0 = Nothing
toBase94 x = Just $ f mempty x where
  f acc 0 = acc
  f acc n = let (d,m) = n `divMod` 94
            in f (word8 (fromInteger m + 33) <> acc) d

-- | Helper function to generate the string lookup function
--   below, we generate this at compile time for efficiency
--   (and yes, we could use Template Haskell for that, but
--    we keep the code simple this time)
generateEncodeString :: IO ()
generateEncodeString = forM_ (zip chars charsDecoded) $ \(c1,c2) ->
  putStrLn $ "  f " <> show c2 <> " = " <> show c1

-- | Convert a string from a term to its ICFP representation
encodeString :: ByteString -> Builder
encodeString = lazyByteString . BS8.map f where
  f 'a' = '!'
  f 'b' = '"'
  f 'c' = '#'
  f 'd' = '$'
  f 'e' = '%'
  f 'f' = '&'
  f 'g' = '\''
  f 'h' = '('
  f 'i' = ')'
  f 'j' = '*'
  f 'k' = '+'
  f 'l' = ','
  f 'm' = '-'
  f 'n' = '.'
  f 'o' = '/'
  f 'p' = '0'
  f 'q' = '1'
  f 'r' = '2'
  f 's' = '3'
  f 't' = '4'
  f 'u' = '5'
  f 'v' = '6'
  f 'w' = '7'
  f 'x' = '8'
  f 'y' = '9'
  f 'z' = ':'
  f 'A' = ';'
  f 'B' = '<'
  f 'C' = '='
  f 'D' = '>'
  f 'E' = '?'
  f 'F' = '@'
  f 'G' = 'A'
  f 'H' = 'B'
  f 'I' = 'C'
  f 'J' = 'D'
  f 'K' = 'E'
  f 'L' = 'F'
  f 'M' = 'G'
  f 'N' = 'H'
  f 'O' = 'I'
  f 'P' = 'J'
  f 'Q' = 'K'
  f 'R' = 'L'
  f 'S' = 'M'
  f 'T' = 'N'
  f 'U' = 'O'
  f 'V' = 'P'
  f 'W' = 'Q'
  f 'X' = 'R'
  f 'Y' = 'S'
  f 'Z' = 'T'
  f '0' = 'U'
  f '1' = 'V'
  f '2' = 'W'
  f '3' = 'X'
  f '4' = 'Y'
  f '5' = 'Z'
  f '6' = '['
  f '7' = '\\'
  f '8' = ']'
  f '9' = '^'
  f '!' = '_'
  f '"' = '`'
  f '#' = 'a'
  f '$' = 'b'
  f '%' = 'c'
  f '&' = 'd'
  f '\'' = 'e'
  f '(' = 'f'
  f ')' = 'g'
  f '*' = 'h'
  f '+' = 'i'
  f ',' = 'j'
  f '-' = 'k'
  f '.' = 'l'
  f '/' = 'm'
  f ':' = 'n'
  f ';' = 'o'
  f '<' = 'p'
  f '=' = 'q'
  f '>' = 'r'
  f '?' = 's'
  f '@' = 't'
  f '[' = 'u'
  f '\\' = 'v'
  f ']' = 'w'
  f '^' = 'x'
  f '_' = 'y'
  f '`' = 'z'
  f '|' = '{'
  f '~' = '|'
  f ' ' = '}'
  f '\n' = '~'
  f c = error $ "Unexpected character in string encoding: " ++ show c
