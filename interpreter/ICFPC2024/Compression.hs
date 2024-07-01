module ICFPC2024.Compression (
  intCompress,
  rleCompress,
) where

import ICFPC2024.AST

import Data.List ( genericLength )
import Data.Containers.ListUtils ( nubOrd )
import Data.ByteString.Char8 ( ByteString )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map

-- | Compress a long string into a big integer together with the decompressing code. This
--   does not guarentee the result is shorter.
intCompress :: ByteString -> ByteString -> Term
intCompress prefix bs = ret where
  uniqueChars = nubOrd $ BS8.unpack bs
  base = genericLength uniqueChars
  charMap = Map.fromList $ zip uniqueChars [0..]
  (st,rm) = BS8.span (==head uniqueChars) bs
  charnum c = Map.findWithDefault 0 c charMap
  num = BS8.foldl (\n c -> n * base + charnum c) 0 rm
  ret = (yComb `tapp` TLam 1 (TLam 2 body)) `tapp` TInt num
  frec = TVar 1
  x = TVar 2
  body = TIf (TBinOp x '=' (TInt 0)) (TString $ BS8.fromStrict $ prefix <> st) $
    TBinOp (frec `tapp` TBinOp x '/' (TInt base)) '.' (toChar $ TBinOp x '%' (TInt base))
  toChar v = TBinOp (TInt 1) 'T' $ TBinOp v 'D' $ TString $ BS8.fromStrict $ BS8.pack uniqueChars

-- | Compress a long string into a big integer by using run-length-encoding. This will
--   only help with many repeated characters
rleCompress :: ByteString -> ByteString -> Term
rleCompress suffix bs = ret where
  uniqueChars = nubOrd $ BS8.unpack bs
  base = genericLength uniqueChars
  groups = BS8.group bs
  maxLen = fromIntegral $ succ $ maximum $ map BS8.length groups
  totalBase = maxLen * base
  charMap = Map.fromList $ zip uniqueChars [0..]
  charnum c = Map.findWithDefault 0 c charMap
  num = foldr (\g n -> n * totalBase + fromIntegral (BS8.length g) * base + charnum (BS8.head g)) 0 groups
  ret = (yComb `tapp` TLam 1 (TLam 2 body)) `tapp` TInt num
  frec = TVar 1
  x = TVar 2
  chari = TBinOp x '%' (TInt base)
  cnti = TBinOp (TBinOp x '/' (TInt base)) '%' (TInt maxLen)
  body = TIf (TBinOp x '=' (TInt 0)) (TString $ BS8.fromStrict suffix) $
    TBinOp (toChar chari) '.' (frec `tapp` nextnum)
  nextnum = TIf (TBinOp (TInt 1) '=' cnti)
    (TBinOp x '/' (TInt totalBase))
    (TBinOp x '-' (TInt base))
  toChar v = TBinOp (TInt 1) 'T' $ TBinOp v 'D' $ TString $ BS8.fromStrict $ BS8.pack uniqueChars
