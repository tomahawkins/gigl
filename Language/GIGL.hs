{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE GADTs             #-}
module Language.GIGL
  (
  -- * Types
    GIGL
  , Program (..)
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
  , var
  , var'
  , array
  -- * Statements
  , intrinsic
  , (<==)
  , case'
  , if'
  , label
  , goto
  , assert
  , assume
  -- * Expressions
  , let'
  , (.==)
  , (./=)
  , mux
  ) where

import MonadLib
import Data.SBV (Boolean (..))
import Data.Word

-- | The monad to capture program statements.
type GIGL a i = StateT (a, Program i) Id

modify :: ((a, Program i) -> (a, Program i)) -> GIGL a i ()
modify f = get >>= set . f

-- | Get the meta data.
getMeta :: GIGL a i a
getMeta = get >>= return . fst

-- | Set the meta data.
setMeta :: a -> GIGL a i ()
setMeta a = modify $ \ (_, p) -> (a, p)

-- | Modify the meta data.
modifyMeta :: (a -> a) -> GIGL a i ()
modifyMeta f = modify $ \ (a, p) -> (f a, p)

data Program a = Program
  { variables :: [(String, Maybe Value)]
  , statement :: Stmt a
  } deriving Show

data Value
  = VBool   Bool
  | VWord64 Word64
  | VPair   Value Value
  deriving Show

class    Value' a      where value :: a -> Value
instance Value' Bool   where value = VBool
instance Value' Word64 where value = VWord64
instance (Value' a, Value' b) => Value' (a, b) where value (a, b) = VPair (value a) (value b)

data Stmt a where
  Null      :: Stmt a
  Seq       :: Stmt a -> Stmt a -> Stmt a
  If        :: E Bool -> Stmt a -> Stmt a -> Stmt a
  Assign    :: Value' a => E a -> E a -> Stmt b
  Label     :: String -> Stmt a
  Goto      :: String -> Stmt a
  Intrinsic :: a -> Stmt a

instance Show a => Show (Stmt a) where
  show a = case a of
    Null         -> ""
    Seq    a b   -> show a ++ show b
    If     _ b c -> "if (...)\n" ++ indent (show b) ++ "else\n" ++ indent (show c)
    Assign (Var a) _ -> a ++ " = ...\n"
    Assign _ _       -> error "Invalid LHS.  Expecting variable, got something else."
    Intrinsic a      -> show a ++ "\n"
    Label a          -> a ++ ":\n"
    Goto  a          -> "goto " ++ a ++ "\n"
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
elaborate a b = snd $ runId $ runStateT (a, Program { variables = [], statement = Null }) b

-- | Declares a variabled with an initial value.
var :: Value' a => String -> Maybe a -> GIGL b i (E a)
var name init = do
  name <- mangle name
  modify $ \ (a, p) -> (a, p { variables = variables p ++ [(name, value' init)] })
  return $ Var name
  where
  value' :: Value' a => Maybe a -> Maybe Value
  value' init = case init of
    Nothing -> Nothing
    Just a  -> Just $ value a

-- | Mange names to ensure variable uniqueness.
mangle :: String -> GIGL a i String
mangle name = do
  (_, Program variables _) <- get
  let vars = map fst variables
      mangle n = if notElem name' vars then name' else mangle (n + 1) where name' = name ++ "_m" ++ show n
  if notElem name vars then return name else return (mangle 0)

-- | Declares a variable and makes an immediate assignment.
var' :: Value' a => String -> E a -> GIGL b i (E a)
var' name expr = do
  v <- var name Nothing
  v <== expr
  return v

-- | Declares a new state array variable.
array :: String -> Integer -> GIGL a i (E (Array b))
array = undefined

-- | Inserts a label to name a section of code.
label :: String -> GIGL a i () -> GIGL a i ()
label name code = stmt (Label name) >> code

-- | Jump to a label.
goto :: String -> GIGL a i ()
goto = stmt . Goto

-- | Case statement with an optional default condition.
case' :: E a -> [(E a -> E Bool, GIGL b i ())] -> Maybe (GIGL b i ()) -> GIGL b i ()
case' a b c = case b of
  [] -> case c of { Nothing -> return (); Just c -> c }
  (pred, stmt) : rest -> if' (pred a) stmt $ case' a rest c

-- | If then else statement.
if' :: E Bool -> GIGL a i () -> GIGL a i () -> GIGL a i ()
if' pred onTrue onFalse = do
  (s, p) <- get
  set (s, p { statement = Null })
  onTrue
  (s, pT) <- get
  set (s, pT { statement = Null })
  onFalse
  (s, pF) <- get
  set (s, Program { variables = variables pF, statement = statement p })
  stmt $ If pred (statement pT) (statement pF)

-- | Adds a statement to the program.
stmt :: Stmt i -> GIGL a i ()
stmt s = modify $ \ (a, p) -> (a, p { statement = Seq (statement p) s }) 

-- | Adds an intrisic statement to the program.
intrinsic :: i -> GIGL a i ()
intrinsic = stmt . Intrinsic

-- | Non recursive let expression.
let' :: String -> E a -> E b -> E b
let' = Let

infix 4 .==, ./=
-- | Equality.
(.==) :: E a -> E a -> E Bool
(.==) = Eq

-- | Inequality.
(./=) :: E a -> E a -> E Bool
a ./= b = bnot $ a .== b

infix 0 <==
-- | Variable assignment.
(<==) :: Value' a => E a -> E a -> GIGL b i ()
a <== b = stmt $ Assign a b

-- | Conditional expression.
mux :: E Bool -> E a -> E a -> E a
mux = Mux

-- | Assert an expression is true.
assert :: String -> E Bool -> GIGL a i ()
assert = undefined

-- | Assume an expression is true.
assume :: String -> E Bool -> GIGL a i ()
assume = undefined

