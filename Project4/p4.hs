{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- AST and Type Definitions
data TERMLANG where
  Num :: Int -> TERMLANG
  Id :: String -> TERMLANG
  Plus :: TERMLANG -> TERMLANG -> TERMLANG
  Minus :: TERMLANG -> TERMLANG -> TERMLANG
  Mult :: TERMLANG -> TERMLANG -> TERMLANG
  Div :: TERMLANG -> TERMLANG -> TERMLANG
  If0 :: TERMLANG -> TERMLANG -> TERMLANG -> TERMLANG
  Lambda :: String -> TERMLANG -> TERMLANG
  App :: TERMLANG -> TERMLANG -> TERMLANG
  deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  ClosureV :: String -> TERMLANG -> ValueEnv -> VALUELANG
  deriving (Show,Eq)
  
type TermEnv = [(String,TERMLANG)]
type ValueEnv = [(String,VALUELANG)]
 
data EXTLANG where
  NumX :: Int -> EXTLANG
  PlusX :: EXTLANG -> EXTLANG -> EXTLANG
  MinusX :: EXTLANG -> EXTLANG -> EXTLANG
  MultX :: EXTLANG -> EXTLANG -> EXTLANG
  DivX :: EXTLANG -> EXTLANG -> EXTLANG
  If0X :: EXTLANG -> EXTLANG -> EXTLANG -> EXTLANG
  LambdaX :: String -> EXTLANG -> EXTLANG
  AppX :: EXTLANG -> EXTLANG -> EXTLANG
  BindX :: String -> EXTLANG -> EXTLANG -> EXTLANG
  IdX :: String -> EXTLANG
  deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: TERMLANG (no bind)

-- Exercise 1:
evalDyn :: TermEnv -> TERMLANG -> (Maybe TERMLANG)
evalDyn _ (Num x) = return (Num x)
evalDyn e (Plus l r) = do { (Num l') <- evalDyn e l;
                            (Num r') <- evalDyn e r;
                            return (Num (l' + r'))}
evalDyn e (Minus l r) = do { (Num l') <- evalDyn e l;
                             (Num r') <- evalDyn e r;
                             if r' < l' then return (Num (l' - r')) else Nothing}
evalDyn e (Mult l r) = do { (Num l') <- evalDyn e l;
                            (Num r') <- evalDyn e r;
                            return (Num (l' * r'))}
evalDyn e (Div l r) = do { (Num l') <- evalDyn e l;
                           (Num r') <- evalDyn e r;
                           if r' == 0 then Nothing else return (Num (l' `div` r'))}
evalDyn e (If0 c t e') = do { (Num c') <- evalDyn e c;
                             t' <- evalDyn e t;
                             e'' <- evalDyn e e';
                             if c' == 0 then return t' else return e''}
evalDyn _ (Lambda i b) = return (Lambda i b)
evalDyn e (App f a) = do { a' <- evalDyn e a;
                           (Lambda i s) <- evalDyn e f;
                           evalDyn ((i, a'):e) s}
evalDyn e (Id x) = lookup x e

-- Exercise 2:
evalStat :: ValueEnv -> TERMLANG -> (Maybe VALUELANG)
evalStat _ (Num x) = return (NumV x)
evalStat e (Plus l r) = do { (NumV l') <- evalStat e l;
                             (NumV r') <- evalStat e r;
                             return (NumV (l' + r'))}
evalStat e (Minus l r) = do { (NumV l') <- evalStat e l;
                              (NumV r') <- evalStat e r;
                              if r' < l' then return (NumV (l' - r')) else Nothing}
evalStat e (Mult l r) = do { (NumV l') <- evalStat e l;
                             (NumV r') <- evalStat e r;
                             return (NumV (l' * r'))}
evalStat e (Div l r) = do { (NumV l') <- evalStat e l;
                            (NumV r') <- evalStat e r;
                            if r' == 0 then Nothing else return (NumV (l' `div` r'))}
evalStat e (If0 c t e') = do { (NumV c') <- evalStat e c;
                               t' <- evalStat e t;
                               e'' <- evalStat e e';
                               if c' == 0 then return t' else return e''}
evalStat e (Lambda i b) = return (ClosureV i b e)
evalStat e (App f a) = do { a' <- evalStat e a;
                            (ClosureV i b e') <- evalStat e f;
                            evalStat ((i, a'):e') b}
evalStat e (Id x) = lookup x e

-- Part 2: Elaboration

-- Exercise 1:
elabTerm :: EXTLANG -> TERMLANG
elabTerm (NumX x) = (Num x)
elabTerm (PlusX l r) = Plus (elabTerm l) (elabTerm r)
elabTerm (MinusX l r) = Minus (elabTerm l) (elabTerm r)
elabTerm (MultX l r) = Mult (elabTerm l) (elabTerm r)
elabTerm (DivX l r) = Div (elabTerm l) (elabTerm r)
elabTerm (If0X c t e) = If0 (elabTerm c) (elabTerm t) (elabTerm e)
elabTerm (LambdaX i b) = Lambda i (elabTerm b)
elabTerm (AppX f a) = App (elabTerm f) (elabTerm a)
elabTerm (BindX i v b) = App (Lambda i (elabTerm b)) (elabTerm v)
elabTerm (IdX x) = Id x 

-- Exercise 2:
evalTerm :: ValueEnv -> EXTLANG -> (Maybe VALUELANG)
evalTerm e t = evalStat e  (elabTerm t)


