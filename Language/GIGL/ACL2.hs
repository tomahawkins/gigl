-- | ACL2 generation.
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module Language.GIGL.ACL2
  ( SExpr (..)
  , acl2
  ) where

import MonadLib

import Language.GIGL

-- | ACL2 generation.
acl2 :: String -> a -> GIGL a () -> SExpr
acl2 name a b = acl2' name $ snd $ elaborate a b

acl2' :: String -> Program -> SExpr
acl2' name p = SA [SV "defun", SV name, SA [SV "vars-in)"], acl2SExpr $ letRewrite vars $ statement p]
  where
  vars = [ v | (v, _) <- variables p ]

letRewrite :: [String] -> Stmt -> E Tuple2
letRewrite vars s = input (zip [0 ..] vars) $ body $ output vars
  where
  (_, _, body) = elaborate (0, [], id) $ stmt s

  input :: [(Int, String)] -> E a -> E a
  input a = case a of
    [] -> id
    (i, v) : rest -> Let v (extract i) . input rest
    where
    extract :: Int -> E a
    extract i
      | i == length vars - 1 =           extract' i
      | otherwise            = ProjFst $ extract' i
    extract' :: Int -> E a
    extract' i
      | i <= 0    = Variable "vars-in"
      | otherwise = ProjSnd $ extract' $ i - 1

  output :: [String] -> E Tuple2
  output vars = case vars of
    [a, b] -> Tuple2' (Variable a) (Variable b)
    a : b  -> Tuple2' (Variable a) (output   b)
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
  Variable   a     -> SV a
  ArrayIndex _ _   -> error "Array Index not supported."
  Let        _ _ _ -> SA [SV "let*", SA lets, b'] where (lets, b') = combineLets a
  Tuple2     a b   -> f2 "cons" a b
  Tuple2'    a b   -> f2 "cons" a b
  ProjFst    a     -> f1 "car" a
  ProjSnd    a     -> f1 "cdr" a
  Const      a     -> acl2Value $ value a
  Add        a b   -> f2 "+" a b
  Not        a     -> f1 "not" a
  And        a b   -> f2 "and" a b
  Or         a b   -> f2 "or" a b
  Imply      a b   -> f2 "implies" a b
  Equiv      a b   -> f2 "equals" a b
  Eq         a b   -> f2 "equals" a b
  Mux        a b c -> f3 "if" a b c
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

acl2Value :: Value -> SExpr
acl2Value a = case a of
  VBool   True  -> SV "true"
  VBool   False -> SV "nil"
  VWord64 a     -> SV $ show a
  VPair   a b   -> SA [SV "cons", acl2Value a, acl2Value b]

type ACL2 = GIGL (Int, [(String, String)], E a -> E a)  -- Id for genvars and environment.

newVar :: ACL2 String
newVar = do
  (n, e, f) <- getMeta
  setMeta (n + 1, e, f)
  return $ "_" ++ show n

newLet :: E a -> ACL2 (E b -> E b)
newLet a = do
  v <- newVar
  return $ Let v a
  
stmt :: Stmt -> ACL2 ()
stmt a = case a of
  Null -> return ()
  Seq a b -> stmt a >> stmt b
  {-
  If a b c -> do
    a' <- newVar
    let' a' a
-}
