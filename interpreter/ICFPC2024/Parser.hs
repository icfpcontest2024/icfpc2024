module ICFPC2024.Parser (
  pTerm,
  ParseError (..),
  fromBase94,
  decodeString,
  generateDecodeString,
) where

import ICFPC2024.AST

import Data.Char ( ord )
import Data.Int ( Int64 )
import Data.ByteString.Lazy.Char8 ( ByteString )
import qualified Data.ByteString.Lazy.Char8 as BS8

import Control.Monad ( forM_ )

-- | Types of errors that can be appear in parsing
--   The 'Int' is the index in the input ByteString
data ParseError
  = UnexpectedChar Char Int64
  | UnexpectedEOF
  | UnusedInput Int64
  deriving ( Show )

-- | Parse a 'Term' from a 'ByteString', with nice error
--   reporting.
pTerm :: ByteString -> Either ParseError Term
pTerm = res where
  -- Build the result by first verifying that we did not
  -- get any strange characters, and the starting the recursion.
  res inp = do
    let incorr = BS8.dropWhile (\c -> ' ' <= c && c <= '~') inp
    if BS8.length incorr /= 0
      then Left $ UnexpectedChar (BS8.head incorr) (BS8.length inp - BS8.length incorr)
      else do
      (ans,rm,idx) <- f inp 0
      case BS8.uncons rm of
        Nothing -> Right ans
        Just _  -> Left $ UnusedInput idx
  -- Recursively go through the input. Note that the input bytestring
  -- is not copied, we just look at it in memory
  f :: ByteString -> Int64 -> Either ParseError (Term, ByteString, Int64)
  f inp idx = case BS8.uncons inp of
    -- Boolean constants
    Just ('F',xs) -> pure (TBool False, xs, idx+1)
    Just ('T',xs) -> pure (TBool True, xs, idx+1)
    -- Integer constants
    Just ('I',xs) -> do
      (y,ys) <- getNonEmpty xs idx
      pure (TInt $ fromBase94 y, ys, idx + 1 + BS8.length y)
    -- String constants
    Just ('S',xs) ->
      let (y,ys) = BS8.break (==' ') xs
      in pure (TString $ decodeString y, ys, idx + 1 + BS8.length y)
    -- Unary op
    Just ('U',xs) -> case BS8.uncons xs of
      Nothing -> Left UnexpectedEOF
      Just (op,ys) -> do
        (t',rs,ridx) <- getNext ys (idx+2)
        pure (TUnOp op t', rs, ridx)
    -- Binary op
    Just ('B',xs) -> case BS8.uncons xs of
      Nothing -> Left UnexpectedEOF
      Just (op,ys) -> do
        (l,zs,idx') <- getNext ys (idx+2)
        (r,rs,ridx) <- getNext zs idx'
        pure (TBinOp l op r, rs, ridx)
    -- If
    Just ('?',xs) -> do
      (b,ys,idx') <- getNext xs (idx+1)
      (tr,zs,idx'') <- getNext ys idx'
      (fl,rs,ridx) <- getNext zs idx''
      pure (TIf b tr fl, rs, ridx)
    -- Variables
    Just ('v',xs) -> do
      (y,ys) <- getNonEmpty xs idx
      pure (TVar $ fromBase94 y, ys, idx + 1 + BS8.length y)
    -- Lambda abstraction
    Just ('L',xs) -> do
      (y,ys) <- getNonEmpty xs idx
      (body, rs, ridx) <- getNext ys (idx + 1 + BS8.length y)
      pure (TLam (fromBase94 y) body, rs, ridx)
    -- Error cases
    Just (c,_) -> Left $ UnexpectedChar c idx
    Nothing -> Left UnexpectedEOF

  -- | This expects a space at the beginning and then an expression
  getNext :: ByteString -> Int64 -> Either ParseError (Term, ByteString, Int64)
  getNext xs idx = case BS8.uncons xs of
    Nothing       -> Left UnexpectedEOF
    Just (' ',ys) -> f ys (idx+1)
    Just (c,_)    -> Left $ UnexpectedChar c idx

  -- | Get all tokens until the next space or end of the string, and error
  --   if that is nonempty
  getNonEmpty :: ByteString -> Int64 -> Either ParseError (ByteString, ByteString)
  getNonEmpty xs idx =
    let (y,ys) = BS8.break (==' ') xs
    in if BS8.null y
    then Left $ case BS8.uncons ys of
      Nothing    -> UnexpectedEOF
      Just (c,_) -> UnexpectedChar c (idx + 1)
    else pure (y, ys)

-- | Convert a base-94 string to its 'Integer' representation
fromBase94 :: ByteString -> Integer
fromBase94 = BS8.foldl f 0 where
  f s c = s * 94 + toInteger (ord c) - 33

-- | Helper function to generate the string lookup function
--   below, we generate this at compile time for efficiency
--   (and yes, we could use Template Haskell for that, but
--    we keep the code simple this time)
generateDecodeString :: IO ()
generateDecodeString = forM_ (zip chars charsDecoded) $ \(c1,c2) ->
  putStrLn $ "  f " <> show c1 <> " = " <> show c2

-- | Convert a string from the ICFP representation to human readable form
decodeString :: ByteString -> ByteString
decodeString = BS8.map f where
  f '!' = 'a'
  f '"' = 'b'
  f '#' = 'c'
  f '$' = 'd'
  f '%' = 'e'
  f '&' = 'f'
  f '\'' = 'g'
  f '(' = 'h'
  f ')' = 'i'
  f '*' = 'j'
  f '+' = 'k'
  f ',' = 'l'
  f '-' = 'm'
  f '.' = 'n'
  f '/' = 'o'
  f '0' = 'p'
  f '1' = 'q'
  f '2' = 'r'
  f '3' = 's'
  f '4' = 't'
  f '5' = 'u'
  f '6' = 'v'
  f '7' = 'w'
  f '8' = 'x'
  f '9' = 'y'
  f ':' = 'z'
  f ';' = 'A'
  f '<' = 'B'
  f '=' = 'C'
  f '>' = 'D'
  f '?' = 'E'
  f '@' = 'F'
  f 'A' = 'G'
  f 'B' = 'H'
  f 'C' = 'I'
  f 'D' = 'J'
  f 'E' = 'K'
  f 'F' = 'L'
  f 'G' = 'M'
  f 'H' = 'N'
  f 'I' = 'O'
  f 'J' = 'P'
  f 'K' = 'Q'
  f 'L' = 'R'
  f 'M' = 'S'
  f 'N' = 'T'
  f 'O' = 'U'
  f 'P' = 'V'
  f 'Q' = 'W'
  f 'R' = 'X'
  f 'S' = 'Y'
  f 'T' = 'Z'
  f 'U' = '0'
  f 'V' = '1'
  f 'W' = '2'
  f 'X' = '3'
  f 'Y' = '4'
  f 'Z' = '5'
  f '[' = '6'
  f '\\' = '7'
  f ']' = '8'
  f '^' = '9'
  f '_' = '!'
  f '`' = '"'
  f 'a' = '#'
  f 'b' = '$'
  f 'c' = '%'
  f 'd' = '&'
  f 'e' = '\''
  f 'f' = '('
  f 'g' = ')'
  f 'h' = '*'
  f 'i' = '+'
  f 'j' = ','
  f 'k' = '-'
  f 'l' = '.'
  f 'm' = '/'
  f 'n' = ':'
  f 'o' = ';'
  f 'p' = '<'
  f 'q' = '='
  f 'r' = '>'
  f 's' = '?'
  f 't' = '@'
  f 'u' = '['
  f 'v' = '\\'
  f 'w' = ']'
  f 'x' = '^'
  f 'y' = '_'
  f 'z' = '`'
  f '{' = '|'
  f '|' = '~'
  f '}' = ' '
  f '~' = '\n'
  f c = error $ "Unexpected character in string decoding: " ++ show c
