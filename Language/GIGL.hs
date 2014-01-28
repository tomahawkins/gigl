{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE GADTs             #-}
module Language.GIGL
  (
  -- * Types
    GIGL
  , Program
  , Value   (..)
  , Value'  (..)
  , E       (..)
  , Stmt    (..)
  , Untyped
  , Boolean (..)
  -- * Program Compilation
  , elaborate
  , getMeta
  , setMeta
  , modifyMeta
  -- * Declarations
  , variables
  , proc
  , var
  , var'
  , bool
  , word64
  , array
  -- * Statements
  , comment
  , intrinsic
  , call
  , (<==)
  , case'
  , if'
  , assert
  , assume
  -- * Expressions
  , let'
  , (.==)
  , (./=)
  , mux
  , mux'
  ) where

import MonadLib
import Data.List (nub, sort)
import Data.SBV (Boolean (..))
import Data.Word

-- | The monad to capture program statements.
type GIGL a i = StateT (a, Program i, Stmt i) Id

modify :: ((a, Program i, Stmt i) -> (a, Program i, Stmt i)) -> GIGL a i ()
modify f = get >>= set . f

-- | Get the meta data.
getMeta :: GIGL a i a
getMeta = do
  (a, _, _) <- get
  return a

-- | Set the meta data.
setMeta :: a -> GIGL a i ()
setMeta a = modify $ \ (_, p, s) -> (a, p, s)

-- | Modify the meta data.
modifyMeta :: (a -> a) -> GIGL a i ()
modifyMeta f = modify $ \ (a, p, s) -> (f a, p, s)

type Program a = [(String, Stmt a)]

data Value
  = VBool   Bool
  | VWord64 Word64
  | VPair   Value Value
  deriving Show

class Value' a where
  value :: a -> Value

instance Value' Bool where
  value = VBool

instance Value' Word64 where
  value = VWord64

instance (Value' a, Value' b) => Value' (a, b) where
  value (a, b) = VPair (value a) (value b)

data Stmt a where
  Null      :: Stmt a
  Seq       :: Stmt a -> Stmt a -> Stmt a
  If        :: E Bool -> Stmt a -> Stmt a -> Stmt a
  Assign    :: E a -> E a -> Stmt b
  Call      :: String -> Stmt a
  Intrinsic :: a -> Stmt a
  Comment   :: String -> Stmt a

instance Show a => Show (Stmt a) where
  show a = case a of
    Comment a    -> "// " ++ a
    Null         -> ""
    Seq    a b   -> show a ++ show b
    If     _ b c -> "if (...)\n" ++ indent (show b) ++ "else\n" ++ indent (show c)
    Assign (Var a) _ -> a ++ " = ...\n"
    Assign _ _       -> error "Invalid LHS.  Expecting variable, got something else."
    Intrinsic a      -> show a ++ "\n"
    Call a           -> a ++ "()"
    where
    indent = unlines . map ("  " ++) . lines

-- | Program expressions.
data E a where 
  Var     :: String -> E a
  Index   :: E (Array a) -> E Word64 -> E a
  Let     :: String -> E b -> E a -> E a
  Untyped :: E a -> E Untyped
  Pair    :: E a -> E b -> E (a, b)
  Fst     :: E (a, b) -> E a
  Snd     :: E (a, b) -> E b
  Const   :: Value' a => a -> E a
  Add     :: E Word64 -> E Word64 -> E Word64
  Not     :: E Bool -> E Bool
  And     :: E Bool -> E Bool -> E Bool
  Or      :: E Bool -> E Bool -> E Bool
  Imply   :: E Bool -> E Bool -> E Bool
  Equiv   :: E Bool -> E Bool -> E Bool
  Eq      :: E a -> E a -> E Bool
  Mux     :: E Bool -> E a -> E a -> E a

data Untyped

data Array a

instance Boolean (E Bool) where
  true  = Const True
  false = Const False
  bnot  = Not
  (&&&) = And
  (|||) = Or
  (==>) = Imply
  (<=>) = Equiv

-- | Elaborate a program.
elaborate :: a -> GIGL a i () -> (a, Program i)
elaborate a' b = (a, p)
  where
  ((), (a, p, _)) = runId $ runStateT (a', [], Null) b

-- | All the variables in a program.
variables :: Program i -> [String]
variables = nub . sort . stmt . foldl1 Seq . snd . unzip
  where
  stmt :: Stmt i -> [String]
  stmt a = case a of
    Comment _    -> []
    Null         -> []
    Seq    a b   -> stmt a ++ stmt b
    If     a b c -> expr a ++ stmt b ++ stmt c
    Assign a b   -> expr a ++ expr b
    Intrinsic _  -> []
    Call _       -> []
  expr :: E a -> [String]
  expr a = case a of
    Var     a     -> [a]
    Index   a b   -> expr a ++ expr b
    Let     _ a b -> expr a ++ expr b
    Untyped a     -> expr a
    Pair    a b   -> expr a ++ expr b
    Fst     a     -> expr a
    Snd     a     -> expr a
    Const   _     -> []
    Add     a b   -> expr a ++ expr b
    Not     a     -> expr a
    And     a b   -> expr a ++ expr b
    Or      a b   -> expr a ++ expr b
    Imply   a b   -> expr a ++ expr b
    Equiv   a b   -> expr a ++ expr b
    Eq      a b   -> expr a ++ expr b
    Mux     a b c -> expr a ++ expr b ++ expr c

-- | Declares a top level procedure.
proc :: String -> GIGL a i () -> GIGL a i ()
proc name proc = do
  (a, p0, s0) <- get
  set (a, p0, Null)
  proc
  (a, p1, s1) <- get
  set (a, p1 ++ [(name, s1)], s0)

-- | Declares a variable.
var :: String -> GIGL b i (E a)
var = return . Var

-- | Declares a variable and makes an immediate assignment.
var' :: String -> E a -> GIGL b i (E a)
var' name expr = do
  v <- var name
  v <== expr
  return v

-- | A boolean variable.
bool :: String -> E Bool
bool = Var

-- | A word64 variable.
word64 :: String -> E Word64
word64 = Var

-- | Declares a new state array variable.
array :: String -> Integer -> GIGL a i (E (Array b))
array = undefined

-- | Case statement with an optional default condition.
case' :: E a -> [(E a -> E Bool, GIGL b i ())] -> Maybe (GIGL b i ()) -> GIGL b i ()
case' a b c = case b of
  [] -> case c of { Nothing -> return (); Just c -> c }
  (pred, stmt) : rest -> if' (pred a) stmt $ case' a rest c

-- | If then else statement.
if' :: E Bool -> GIGL a i () -> GIGL a i () -> GIGL a i ()
if' pred onTrue onFalse = do
  (a, p, s0) <- get
  set (a, p, Null)
  onTrue
  (a, p, s1) <- get
  set (a, p, Null)
  onFalse
  (a, p, s2) <- get
  set (a, p, s0)
  stmt $ If pred s1 s2

-- | Adds a statement to the program.
stmt :: Stmt i -> GIGL a i ()
stmt s' = modify $ \ (a, p, s) -> (a, p, Seq s s') 

-- | Call a procedure.
call :: String -> GIGL a i ()
call = stmt . Call

-- | Adds an intrisic statement to the program.
intrinsic :: i -> GIGL a i ()
intrinsic = stmt . Intrinsic

-- | Add a comment.
comment :: String -> GIGL a i ()
comment = stmt . Comment

-- | Non recursive let expression.
let' :: String -> E a -> E b -> E b
let' = Let

infix 4 .==
-- | Equality.
(.==) :: E a -> E a -> E Bool
(.==) = Eq

infix 4 ./=
-- | Inequality.
(./=) :: E a -> E a -> E Bool
a ./= b = bnot $ a .== b

infixr 0 <==
-- | Variable assignment.
(<==) :: E a -> E a -> GIGL b i ()
a <== b = stmt $ Assign a b

-- | Conditional expression.
mux :: E Bool -> E a -> E a -> E a
mux = Mux

-- | Muxing over a list of predicates with a default condition.
mux' :: [(E Bool, E b)] -> E b -> E b
mux' a def = case a of
  [] -> def
  (pred, val) : rest -> mux pred val $ mux' rest def

-- | Assert an expression is true.
assert :: String -> E Bool -> GIGL a i ()
assert = undefined

-- | Assume an expression is true.
assume :: String -> E Bool -> GIGL a i ()
assume = undefined

