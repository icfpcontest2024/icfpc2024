module ICFPC2024.Interpreter (
  interpret,
  InterpreterError (..),
  ) where

import ICFPC2024.AST ( Term (..) )
import ICFPC2024.Parser ( fromBase94, decodeString )
import ICFPC2024.Printer ( toBase94, encodeString )
import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef )
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BS8
import Control.Monad.Trans (lift )
import Control.Monad.ST ( ST, runST )
import Control.Monad.Except ( ExceptT, throwError, runExceptT )
import Data.Map ( Map )
import qualified Data.Map as Map

-- | Max number of beta reduction steps that we do for contestant-submitted expressions.
maxSteps :: Integer
maxSteps = 10_000_000

-- | Errors that can occur during evaluation
data InterpreterError
  = BetaReductionLimit
  | ScopeError
  | TypeError
  | ArithmeticError
  | UnknownUnOp Char
  | UnknownBinOp Char
  deriving ( Show )

-- | For lazy evaluation
data Thunk s
  = Value (Value s) Integer     -- was evaluated, in the given number of steps
  | Thunk Term (Env s)     -- still needs to be evaluated
  | LazyThunk Term (Env s) -- still needs to be evaluated, but steps only count once

data Value s
  = VInt Integer
  | VBool Bool
  | VString Builder -- Use Builder internally, to have efficient concatenation
  | VClosure Integer Term (Env s)

type Env s = Map Integer (STRef s (Thunk s))

-- | Interpreter, which uses STRef as mutable references to represent
--   thunks, so that we evaluate each thunk at most once.
interpret :: Bool -> Term -> Either InterpreterError (Term, Integer)
interpret checkMax term = runST run where
  -- We need the type signature here to fix 's'
  run :: forall s. ST s (Either InterpreterError (Term, Integer))
  run = do
    stepr <- newSTRef 0

    -- Helper for increasing the steps and checking the max
    let incSteps :: Integer -> ExceptT InterpreterError (ST s) ()
        incSteps x = do
          s <- lift $ readSTRef stepr
          let s' = s + x
          if checkMax && s' > maxSteps
            then throwError BetaReductionLimit
            else lift $ writeSTRef stepr s'

    -- Locally define eval so that incSteps / stepr are in scope
    let eval :: Term -> Env s -> ExceptT InterpreterError (ST s) (Value s)
        -- Constants
        eval (TInt i)    _ = pure $ VInt i
        eval (TString s) _ = pure $ VString $ lazyByteString s
        eval (TBool b)   _ = pure $ VBool b
        eval (TLam v b) vm = pure $ VClosure v b vm

        -- Variables, which may trigger extra computation
        eval (TVar v) vm = case v `Map.lookup` vm of
          Nothing -> throwError ScopeError
          Just ref -> lift (readSTRef ref) >>= \case
            -- Was already evaluated, we fake call-by-name by counting the extra steps again
            Value t' es -> do
              incSteps es
              pure t'
            -- Still need to be evaluated, we do so and store the result
            Thunk t' vm' -> do
              sbefore <- lift $ readSTRef stepr
              t'' <- eval t' vm'
              safter <- lift $ readSTRef stepr
              lift $ writeSTRef ref $ Value t'' (safter - sbefore)
              pure t''
            -- Similar to thunk, but once evaluated, we don't count extra steps
            LazyThunk t' vm' -> do
              t'' <- eval t' vm'
              lift $ writeSTRef ref $ Value t'' 0
              pure t''
      
        -- Call by name beta reduction
        eval (TBinOp l '$' r) vm = eval l vm >>= \case
          VClosure v l' vm' -> do
            incSteps 1
            th <- lift $ newSTRef $ Thunk r vm
            let vm'' = Map.insert v th vm'
            eval l' vm''
          _ -> throwError TypeError

        -- Call by value (strict) beta reduction
        eval (TBinOp l '!' r) vm = do
          r' <- eval r vm
          incSteps 1
          eval l vm >>= \case
            VClosure v l' vm' -> do
              th <- lift $ newSTRef $ Value r' 0
              let vm'' = Map.insert v th vm'
              eval l' vm''
            _ -> throwError TypeError

        -- Call by need (lazy) beta reduction
        eval (TBinOp l '~' r) vm = eval l vm >>= \case
          VClosure v l' vm' -> do
            incSteps 1
            th <- lift $ newSTRef $ LazyThunk r vm
            let vm'' = Map.insert v th vm'
            eval l' vm''
          _ -> throwError TypeError

        -- Unary operator
        eval (TUnOp c t) vm = case c `lookup` unOps of
          Nothing -> throwError $ UnknownUnOp c
          Just uf -> do
            t' <- eval t vm
            either throwError pure $ uf t'

        -- Binary operator
        eval (TBinOp l c r) vm = case c `lookup` binOps of
          Nothing -> throwError $ UnknownBinOp c
          Just bf -> do
            l' <- eval l vm
            r' <- eval r vm
            either throwError pure $ bf l' r'

        -- If statements
        eval (TIf tb tt tf) vm = eval tb vm >>= \case
          VBool True  -> eval tt vm
          VBool False -> eval tf vm
          _ -> throwError TypeError

    -- Convert internal 'Value' to 'Term' again
    let toTerm (VInt i)         = TInt i
        toTerm (VBool b)        = TBool b
        toTerm (VString s)      = TString $ toLazyByteString s
        toTerm (VClosure v t _) = TLam v t
    
    -- Kick off the computation an get construct the final result
    mbR <- runExceptT $ eval term Map.empty
    steps <- readSTRef stepr
    pure $ case mbR of
      Left e  -> Left e
      Right r -> Right (toTerm r, steps)

unOps :: [(Char, Value s -> Either InterpreterError (Value s))]
unOps = r where
  r =
    [ ('!', boolOp $ Right . VBool . not)
    , ('-', intOp $ Right . VInt . negate)
    , ('#', stringOp $ Right . VInt . fromBase94 . toLazyByteString . encodeString . toLazyByteString)
    , ('$', intOp $ \i -> case toBase94 i of
                            Just b94 -> Right $ VString $ lazyByteString $ decodeString $ toLazyByteString b94
                            Nothing -> Left ArithmeticError
      )
    ]
  boolOp f (VBool b) = f b
  boolOp _ _ = Left TypeError
  
  stringOp f (VString b) = f b
  stringOp _ _ = Left TypeError
  
  intOp f (VInt b) = f b
  intOp _ _ = Left TypeError

binOps :: [(Char, Value s -> Value s -> Either InterpreterError (Value s))]
binOps = r where
  r =
    [ ('+', intOp $ \a b -> pure $ VInt (a + b))
    , ('-', intOp $ \a b -> pure $ VInt (a - b))
    , ('*', intOp $ \a b -> pure $ VInt (a * b))
    , ('/', intOp $ \a b -> if b == 0 then Left ArithmeticError else Right $ VInt (a `quot` b))
    , ('%', intOp $ \a b -> if b == 0 then Left ArithmeticError else Right $ VInt (a `rem` b))
    , ('<', intOp $ \a b -> pure $ VBool (a < b))
    , ('>', intOp $ \a b -> pure $ VBool (a > b))
    , ('=', eqOp )
    , ('|', boolOp $ \a b -> pure $ VBool (a || b))
    , ('&', boolOp $ \a b -> pure $ VBool (a && b))
    , ('.', stringOp $ \a b -> pure $ VString (a <> b))
    , ('T', intStringOp $ \a b -> pure $ VString (lazyByteString $ BS8.take (fromInteger a) $ toLazyByteString b))
    , ('D', intStringOp $ \a b -> pure $ VString (lazyByteString $ BS8.drop (fromInteger a) $ toLazyByteString b))
    ]
  
  eqOp (VBool a)   (VBool b)   = pure $ VBool $ a == b
  eqOp (VInt a)    (VInt b)    = pure $ VBool $ a == b
  eqOp (VString a) (VString b) = pure $ VBool $ toLazyByteString a == toLazyByteString b
  eqOp _ _ = Left TypeError
  
  intOp f (VInt a) (VInt b) = f a b
  intOp _ _ _ = Left TypeError

  boolOp f (VBool a) (VBool b) = f a b
  boolOp _ _ _ = Left TypeError

  stringOp f (VString a) (VString b) = f a b
  stringOp _ _ _ = Left TypeError

  intStringOp f (VInt a) (VString b) = f a b
  intStringOp _ _ _ = Left TypeError
