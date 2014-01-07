{-# OPTIONS_GHC -W #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE GADTs             #-}
module Language.GIGL
  (
  -- * Types
    GIGL
  , Program (..)
  , E       (..)
  , Label
  , Boolean (..)
  -- * Program Elaboration
  , elaborate
  -- * Language Declarations
  , var
  , array
  , function
  -- * Language Mechanisms
  , case'
  , if'
  , goto
  , (.==)
  , (./=)
  , mux
  , (<==)
  , assert
  , assume
  ) where

import MonadLib hiding (Label)
import Data.SBV (Boolean (..))
import Data.Word

-- | The monad to capture program statements.
type GIGL a = StateT (a, Program) IO

data Program = Program

-- | Program expressions.
data E a where 
  Variable   :: String -> E a
  ArrayIndex :: E (Array a) -> E Word64 -> E a
  Tuple2     :: E a -> E b -> E (a, b)
  ProjFst    :: E (a, b) -> E a
  ProjSnd    :: E (a, b) -> E b
  Const      :: a -> E a
  Add        :: E Word64 -> E Word64 -> E Word64
  Not        :: E Bool -> E Bool
  And        :: E Bool -> E Bool -> E Bool
  Or         :: E Bool -> E Bool -> E Bool
  Imply      :: E Bool -> E Bool -> E Bool
  Equiv      :: E Bool -> E Bool -> E Bool
  Eq         :: E a -> E a -> E Bool
  Mux        :: E Bool -> E a -> E a -> E a

data Array a

instance Boolean (E Bool) where
  true  = Const True
  false = Const False
  bnot  = Not
  (&&&) = And
  (|||) = Or
  (==>) = Imply
  (<=>) = Equiv


-- | Labels.
type Label = String

-- | Elaborate a program.
elaborate :: a -> GIGL a () -> IO (a, Program)
elaborate a b = runStateT (a, Program) b >>= return . snd

class    Var a      where var :: String -> Maybe a -> GIGL b (E a)
instance Var Bool   where var = undefined
instance Var Word64 where var = undefined
instance (Var a, Var b) => Var (a, b) where var = undefined

-- | Declares a new state array variable.
array :: String -> Integer -> GIGL a (E (Array b))
array = undefined

-- | Declares a top level function.
--   Functions aren't really functions; they don't take arguments and they don't return results.
function :: Label -> GIGL a () -> GIGL a ()
function = undefined

-- | Jump to a label.
goto :: Label -> GIGL a ()
goto = undefined

-- | Case statement with no default.
case' :: E a -> [(E a -> E Bool, GIGL b ())] -> GIGL b ()
case' a b = case b of
  [] -> return ()
  (pred, stmt) : rest -> if' (pred a) stmt $ case' a rest

-- | If then else statement.
if' :: E Bool -> GIGL a () -> GIGL a () -> GIGL a ()
if' = undefined

infix 4 .==, ./=
-- | Equality.
(.==) :: E a -> E a -> E Bool
(.==) = Eq

-- | Inequality.
(./=) :: E a -> E a -> E Bool
a ./= b = bnot $ a .== b

infix 0 <==
-- | Variable assignment.
(<==) :: E a -> E a -> GIGL b ()
(<==) = undefined

-- | Conditional expression.
mux :: E Bool -> E a -> E a -> E a
mux = Mux

-- | Assert an expression is true.
assert :: String -> E Bool -> GIGL a ()
assert = undefined

-- | Assume an expression is true.
assume :: String -> E Bool -> GIGL a ()
assume = undefined
