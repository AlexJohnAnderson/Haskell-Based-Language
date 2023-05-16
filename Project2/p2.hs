{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TYPELANG = TNum
              | TBool 
              deriving (Show,Eq)

data TERMLANG = Num Int 
              | Plus TERMLANG TERMLANG 
              | Minus TERMLANG TERMLANG 
              | Mult TERMLANG TERMLANG 
              | Div TERMLANG TERMLANG 
              | Boolean Bool 
              | And TERMLANG TERMLANG 
              | Or TERMLANG TERMLANG 
              | Leq TERMLANG TERMLANG 
              | IsZero TERMLANG 
              | If TERMLANG TERMLANG TERMLANG 
              deriving (Show,Eq)

-------------------------------
------ Project Exercises ------
-------------------------------
-- Part 1: Evaluation Functions

-- Exercise 1
evalM :: TERMLANG -> Maybe TERMLANG
evalM (Num x) = if x<0 then Nothing else Just (Num x)
evalM (Plus l r) = do {(Num l') <- evalM l;
                       (Num r') <- evalM r;
                       return (Num(l' + r'))}

evalM (Minus l r) = do {(Num l') <- evalM l;
                        (Num r') <- evalM r;
                        if (l'- r') < 0 then Nothing else return (Num(l' - r'))}

evalM (Mult l r) = do {(Num l') <- evalM l;
                       (Num r') <- evalM r;
                       return (Num(l' * r'))}

evalM (Div l r) = do {(Num l') <- evalM l;
                      (Num r') <- evalM r;
                      if r' == 0 then Nothing else return (Num(l' `div` r'))}

evalM (Boolean b) = (Just (Boolean b))

evalM (And t1 t2) = do { Boolean r1 <- (evalM t1) ;
                         Boolean r2 <- (evalM t2) ;
                         return (Boolean(r1 && r2))}

evalM (Leq t1 t2) = do { Boolean r1 <- (evalM t1) ;
                         Boolean r2 <- (evalM t2) ;
                         return (Boolean(r1 <= r2))}

evalM (IsZero t) = do { r <- (evalM t) ;
                        return (Boolean(r == (Num 0)))}

evalM (If t1 t2 t3) = do { (Boolean v) <- (evalM t1) ;
                           (if v then (evalM t2) else (evalM t3))}

-- Exercise 2
typeofM :: TERMLANG -> Maybe TYPELANG
typeofM (Num n) = if n >= 0 then return TNum else Nothing
typeofM (Boolean b) = return TBool
typeofM (Plus l r) = do {TNum <- (typeofM l);
                         TNum <- (typeofM r);
                         return TNum }

typeofM (Minus l r) = do {TNum <- (typeofM l);
                          TNum <- (typeofM r);
                          return TNum}

typeofM (Mult l r) = do{TNum <- (typeofM l);
                        TNum <- (typeofM r);
                        return TNum}

typeofM (Div l r) = do{TNum <- (typeofM l);
                       TNum <- (typeofM r);
                       return TNum}

typeofM (And l r) = do{TBool <- (typeofM l);
                       TBool <- (typeofM r);
                       return TBool}

typeofM (Leq l r) = do{TNum <- (typeofM l);
                       TNum <- (typeofM r);
                       return TBool}

typeofM (IsZero t) = do{TNum <- (typeofM t);
                        return TBool}

typeofM (If c t e) = do{TBool <- (typeofM c);
                        t' <- (typeofM t);
                        e' <- (typeofM e);
                        if t' == e';
                         then (return t');
                         else Nothing}

-- Exercise 3
interpTypeEval :: TERMLANG -> Maybe TERMLANG
interpTypeEval e = let e' = typeofM e in
                   do{e'' <- evalM e;
                      return e''}
-- Part 2: Optimizer

-- Exercise 1
optimize :: TERMLANG -> TERMLANG
optimize (Plus t1 (Num 0)) = t1
optimize (Plus (Num 0) t2) = t2
optimize (If (Boolean True) t1 t2) = t1
optimize (If (Boolean False) t1 t2) = t2
--this catches all non-special cases of optimize
optimize a = a

-- Exercise 2
interpOptEval :: TERMLANG -> Maybe TERMLANG
interpOptEval e = let e' = optimize e in
                  do{e'' <- evalM e';
                     return e''}