-- | ACL2 generation.
{-# LANGUAGE GADTs      #-}
module Language.GIGL.ACL2
  ( SExpr (..)
  , acl2
  ) where

import Data.List (nub)
import Data.Maybe (fromJust)
import MonadLib
import Language.GIGL

-- | ACL2 generation.
acl2 :: String -> a -> GIGL a () () -> [SExpr]
acl2 name a b = acl2' name $ snd $ elaborate a b

acl2' :: String -> Program () -> [SExpr]
acl2' name p = 
  [ SA [SV "set-ignore-ok", SV ":warn"]
  --, SA [SV "defun", SV $ name ++ "-init", SA [SV "vars-in"], acl2SExpr $ initialConditions $ variables p]
  , SA [SV "defun", SV name,              SA [SV "vars-in"], acl2SExpr $ letRewrite (variables p) $ statement p]
  ]

letRewrite :: [String] -> Stmt () -> E Untyped
letRewrite vars s = inputProject vars $ body $ outputTuple vars
  where
  body :: E Untyped -> E Untyped
  ((), (_, _, body)) = runId $ runStateT (0, zip vars vars, id) $ stmt s >> bindOutputs vars

-- | Initial condition relation.
{-
initialConditions :: [(String, Maybe Value)] -> E Bool
initialConditions vars = inputProject (fst $ unzip vars) $ foldl (&&&) true [ init (Var name .==) value | (name, Just value) <- vars ]
  where
  init :: (E Untyped -> E Bool) -> Value -> E Bool
  init f value = case value of
    VBool   a   -> f $ Untyped (Const a)
    VWord64 a   -> f $ Untyped (Const a)
    VPair   a b -> init (\ a -> init (\ b -> f $ Untyped $ Pair a b) b) a
    -}

-- | Variable projection from input tuple.
inputProject :: [String] -> E a -> E a
inputProject vars = input $ zip [0 ..] vars
  where
  input :: [(Int, String)] -> E a -> E a
  input a = case a of
    [] -> id
    (i, v) : rest -> Let v (extract i) . input rest
  extract :: Int -> E a
  extract i
    | i == length vars - 1 =       extract' i
    | otherwise            = Fst $ extract' i
  extract' :: Int -> E a
  extract' i
    | i <= 0    = Var "vars-in"
    | otherwise = Snd $ extract' $ i - 1

-- | Creates an untyped tuple for the output of variables.
outputTuple :: [String] -> E Untyped
outputTuple vars = case vars of
  [a, b] -> Untyped $ Pair (Var a) (Var b)
  a : b  -> Untyped $ Pair (Var a) (outputTuple b)
  _ -> error "Expecting at least 2 variables."

data SExpr
  = SV String
  | SA [SExpr]

instance Show SExpr where
  show a = case a of
    SV a -> a ++ "\n"
    SA args -> "( " ++ indent (concatMap show args) ++ ")\n"
      where
      indent = drop 2 . unlines . map ("  " ++) . lines

acl2SExpr :: E a -> SExpr
acl2SExpr a = case a of
  Var     a     -> SV a
  Index   _ _   -> error "Array Index not supported."
  Let     _ _ _ -> SA [SV "let*", SA lets, b'] where (lets, b') = combineLets a
  Untyped a     -> f a
  Pair    a b   -> f2 "cons" a b
  Fst     a     -> f1 "car" a
  Snd     a     -> f1 "cdr" a
  Const   a     -> acl2Value $ value a
  Add     a b   -> f2 "+" a b
  Not     a     -> f1 "not" a
  And     _ _   -> SA $ SV "and" : ands a
  Or      _ _   -> SA $ SV "or"  : ors  a
  Imply   a b   -> f2 "implies" a b
  Equiv   a b   -> f2 "equal" a b
  Eq      a b   -> f2 "equal" a b
  Mux     a b c -> f3 "if" a b c
  where
  f :: E a -> SExpr
  f = acl2SExpr
  f1 :: String -> E a -> SExpr
  f1 a b = SA [SV a, f b]
  f2 :: String -> E a -> E b -> SExpr
  f2 a b c = SA [SV a, f b, f c] 
  f3 :: String -> E a -> E b -> E c -> SExpr
  f3 a b c d = SA [SV a, f b, f c, f d] 
  combineLets :: E a -> ([SExpr], SExpr)
  combineLets a = case a of
    Let v a b -> (SA [SV v, f a] : lets, b')
      where
      (lets, b') = combineLets b
    a -> ([], f a)
  ands :: E Bool -> [SExpr]
  ands a = case a of
    And (Const True) a -> ands a
    And a (Const True) -> ands a
    And a b -> ands a ++ ands b
    a -> [f a]
  ors :: E Bool -> [SExpr]
  ors a = case a of
    Or (Const False) a -> ors a
    Or a (Const False) -> ors a
    Or a b -> ors a ++ ors b
    a -> [f a]
    

acl2Value :: Value -> SExpr
acl2Value a = case a of
  VBool   True  -> SV "true"
  VBool   False -> SV "nil"
  VWord64 a     -> SV $ show a
  VPair   a b   -> SA [SV "cons", acl2Value a, acl2Value b]

type ACL2 = StateT (Int, [(String, String)], E Untyped -> E Untyped) Id  -- Id for genvars and environment.
-- Map from var names to new var names.

renameVars :: (String -> Maybe String) -> E a -> E a
renameVars rename a = case a of
  Var     a     -> case rename a of { Nothing -> Var a; Just a -> Var a }
  Index   a b   -> Index   (f a) (f b)
  Let     a b c -> Let     a (f b) (f c)
  Untyped a     -> Untyped (f a)
  Pair    a b   -> Pair    (f a) (f b)
  Fst     a     -> Fst     (f a)
  Snd     a     -> Snd     (f a)
  Const   a     -> Const   a
  Add     a b   -> Add     (f a) (f b)
  Not     a     -> Not     (f a)
  And     a b   -> And     (f a) (f b)
  Or      a b   -> Or      (f a) (f b)
  Imply   a b   -> Imply   (f a) (f b)
  Equiv   a b   -> Equiv   (f a) (f b)
  Eq      a b   -> Eq      (f a) (f b)
  Mux     a b c -> Mux     (f a) (f b) (f c)
  where
  f :: E a -> E a
  f = renameVars rename

newVar :: ACL2 String
newVar = do
  (n, e, f) <- get
  set (n + 1, e, f)
  return $ "_" ++ show n

newLet :: Maybe String -> E a -> ACL2 (E a)
newLet var a = do
  v <- newVar
  (n, e, f) <- get
  set (n, case var of { Nothing -> e; Just v' -> (v', v) : e }, f . Let v (renameVars (flip lookup e) a))
  return $ Var v

stmt :: Stmt () -> ACL2 ()
stmt a = case a of
  Null -> return ()
  Seq a b -> stmt a >> stmt b
  Intrinsic () -> return ()
  Assign (Var v) e -> do
    newLet (Just v) e
    return ()
  Assign _ _ -> error "Unexpected LHS of assignment (non-variable)."
  Label _ -> return ()
  Goto  _ -> error "Goto statements not supported in ACL2 generation."
  If pred a b -> do
    pred <- newLet Nothing pred
    (i0, e0, f0) <- get
    set (i0, e0, id)
    stmt a
    (i1, e1, f1) <- get
    set (i1, e0, id)
    stmt b
    (i2, e2, f2) <- get
    set (i2, e0, f0 . f1 . f2)
    let v1 = fst $ unzip $ take (length e1 - length e0) e1
        v2 = fst $ unzip $ take (length e2 - length e0) e2
        varsModified = nub $ v1 ++ v2
        mux' v = newLet (Just v) (mux pred (Var $ fromJust $ lookup v e1) (Var $ fromJust $ lookup v e2))
    mapM_ mux' varsModified

bindOutputs :: [String] -> ACL2 ()
bindOutputs = mapM_ bindOutput
  where
  bindOutput :: String -> ACL2 ()
  bindOutput var = do
    (n, e, f) <- get
    let v = fromJust $ lookup var e
    set (n, (var, var) : e, f . Let var (Var v))

