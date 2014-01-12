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
  , Label
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
  , function
  -- * Statements
  , (<==)
  , case'
  , if'
  , goto
  , assert
  , assume
  -- * Expressions
  , let'
  , (.==)
  , (./=)
  , mux
  ) where

import MonadLib hiding (Label)
import Data.SBV (Boolean (..))
import Data.Word

-- | The monad to capture program statements.
type GIGL a = StateT (a, Program) Id

modify :: ((a, Program) -> (a, Program)) -> GIGL a ()
modify f = get >>= set . f

-- | Get the meta data.
getMeta :: GIGL a a
getMeta = get >>= return . fst

-- | Set the meta data.
setMeta :: a -> GIGL a ()
setMeta a = modify $ \ (_, p) -> (a, p)

-- | Modify the meta data.
modifyMeta :: (a -> a) -> GIGL a ()
modifyMeta f = modify $ \ (a, p) -> (f a, p)

data Program = Program
  { variables :: [(String, Maybe Value)]
  , statement :: Stmt
  }

data Value
  = VBool   Bool
  | VWord64 Word64
  | VPair   Value Value

class    Value' a      where value :: a -> Value
instance Value' Bool   where value = VBool
instance Value' Word64 where value = VWord64
instance (Value' a, Value' b) => Value' (a, b) where value (a, b) = VPair (value a) (value b)

data Stmt where
  Null   :: Stmt
  Seq    :: Stmt -> Stmt -> Stmt
  If     :: E Bool -> Stmt -> Stmt -> Stmt
  Assign :: Value' a => E a -> E a -> Stmt

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


-- | Labels.
type Label = String

-- | Elaborate a program.
elaborate :: a -> GIGL a () -> (a, Program)
elaborate a b = snd $ runId $ runStateT (a, Program { variables = [], statement = Null }) b

-- | Declares a variabled with an initial value.
var :: Value' a => String -> Maybe a -> GIGL b (E a)
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
mangle :: String -> GIGL a String
mangle name = do
  (_, Program variables _) <- get
  let vars = map fst variables
      mangle n = if notElem name' vars then name' else mangle (n + 1) where name' = name ++ "_m" ++ show n
  if notElem name vars then return name else return (mangle 0)

-- | Declares a variable and makes an immediate assignment.
var' :: Value' a => String -> E a -> GIGL b (E a)
var' name expr = do
  v <- var name Nothing
  v <== expr
  return v

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
stmt :: Stmt -> GIGL a ()
stmt s = modify $ \ (a, p) -> (a, p { statement = Seq (statement p) s }) 

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
(<==) :: Value' a => E a -> E a -> GIGL b ()
a <== b = stmt $ Assign a b

-- | Conditional expression.
mux :: E Bool -> E a -> E a -> E a
mux = Mux

-- | Assert an expression is true.
assert :: String -> E Bool -> GIGL a ()
assert = undefined

-- | Assume an expression is true.
assume :: String -> E Bool -> GIGL a ()
assume = undefined

