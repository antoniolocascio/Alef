module Common where

import           Utils.Symbol
import           Utils.Set

import           Operation
import           Types
import           AST
import           TypeChecker
import           TypeEnv

title :: Show a => a -> String
title i = "--------------- Test " ++ show i ++ " -----------------"

testHeader :: Show a => a -> IO ()
testHeader i = putStrLn $ title i

green :: String -> String
green s = "\x1b[32m" ++ s ++ "\x1b[0m"

red :: String -> String
red s = "\x1b[31m" ++ s ++ "\x1b[0m"

pr :: Operation
pr = toSymbol "Print"

th :: Operation
th = toSymbol "Throw"

opi :: Int -> Operation
opi i = toSymbol $ "Op" ++ show i

sigma :: [(Operation, OpType)]
sigma = (th, TOp TNat TUnit) : (pr, TOp TNat TUnit) : map
  (\i -> (opi i, TOp TUnit TUnit))
  [1 .. 4]

gop :: Operation -> Exp -> Comp
gop op e = let z = toSymbol "z" in COp op e z (CVal (EVar z))

one :: Exp
one = ESucc EZero

two :: Exp
two = ESucc one

x :: Symbol
x = toSymbol "x"

k :: Symbol
k = toSymbol "k"

toComp :: Term -> Comp
toComp (C c) = c


p1 :: Term
p1 = C $ CLet (toSymbol "x") (gop pr one) (CVal EZero)
t1 :: Type
t1 = CT $ TComp TNat (insert pr empty, toSymbol "mu0")
-- t1 = CT $ TComp TNat (empty, toSymbol "mu0")
e1 :: EnvC
e1 = initEnv sigma

p2 :: Term
p2 = C $ CWith (EVar $ toSymbol "h") (CApp (EVar $ toSymbol "c") EUnit)
e2 :: EnvC
e2 =
  let ht = VT $ THand
        (TComp TNat (insert (opi 1) (insert (opi 2) empty), toSymbol "mu1"))
        (TComp TNat (insert (opi 4) empty, toSymbol "mu1"))
      ct = VT $ TFunc
        TUnit
        (TComp TNat (insert (opi 1) (insert (opi 3) empty), toSymbol "mu2"))
  in  extEnv (toSymbol "c") (Left ct)
        $ extEnv (toSymbol "h") (Left ht) (initEnv sigma)


p3_1 :: Term
p3_1 = E $ EFunc (toSymbol "f") (CApp (EVar (toSymbol "f")) EUnit)
t3_1 :: Type
t3_1 = VT $ TFunc (TFunc TUnit (TComp TUnit (empty, toSymbol "mu1")))
                  (TComp TUnit (empty, toSymbol "mu1"))
-- Error: Effect capture is lost.
-- t3_1 = VT $ TFunc (TFunc TUnit (TComp TUnit (empty, toSymbol "mu1")))
                  -- (TComp TUnit (empty, toSymbol "mu2"))
e3_1 :: EnvC
e3_1 = initEnv sigma

p3_2 :: Term
p3_2 = C $ CApp (EVar (toSymbol "apply")) (EVar (toSymbol "crash"))
e3_2 :: EnvC
e3_2 =
  let applyt = VT $ TFunc (TFunc TUnit (TComp TUnit (empty, toSymbol "mu1")))
                          (TComp TUnit (empty, toSymbol "mu1"))
      crasht = VT $ TFunc TUnit (TComp TUnit (insert th empty, toSymbol "mu2"))
  in  extEnv (toSymbol "crash") (Left crasht)
        $ extEnv (toSymbol "apply") (Left applyt) (initEnv sigma)

p4 :: Term
p4 = E $ EHand
  x
  (CVal EUnit)
  [(pr, x, k, CLet (toSymbol "y") (CApp (EVar k) EUnit) (gop pr (EVar x)))]
t4 :: Type
t4 = VT $ THand (TComp TUnit (insert pr empty, toSymbol "mu1"))
                (TComp TUnit (insert pr empty, toSymbol "mu1"))
-- Error: Doen't capture print operation.
-- t4 = VT $ THand (TComp TUnit (insert pr empty, toSymbol "mu1"))
                -- (TComp TUnit (empty, toSymbol "mu1"))
-- Error: Different effect variable.
-- t4 = VT $ THand (TComp TUnit (insert pr empty, toSymbol "mu1"))
                -- (TComp TUnit (insert pr empty, toSymbol "mu2"))
-- Error: Throw not present in resulting computation type.
-- t4 = VT $ THand (TComp TUnit (insert th $ insert pr empty, toSymbol "mu1"))
                -- (TComp TUnit (insert pr empty, toSymbol "mu1"))
e4 :: EnvC
e4 = initEnv sigma

c5 :: Comp
c5 = CWith
  (EAnno
    (EHand
      x
      (CVal EUnit)
      [(pr, x, k, CLet (toSymbol "y") (CApp (EVar k) EUnit) (gop pr (EVar x)))]
    )
    (THand (TComp TUnit (insert pr empty, toSymbol "mu1"))
           (TComp TUnit (insert pr empty, toSymbol "mu1"))
    )
  )
  (CLet (toSymbol "x")
        (gop pr one)
        (CLet (toSymbol "y") (gop pr two) (CVal EZero))
  )
e5 :: EnvC
e5 = initEnv sigma

-- Take a function and ignore it. (Its type can repeat the effect variable). 
p6 :: Term
p6 = E $ EFunc (toSymbol "f") (CVal EUnit)
t6 :: Type
t6 = VT $ TFunc (TFunc TUnit (TComp TUnit (empty, toSymbol "mu1")))
                (TComp TUnit (empty, toSymbol "mu1"))
e6 :: EnvC
e6 = initEnv sigma

p7 :: Term
p7 = C $ CApp
  (EAnno (EFunc x (CVal EZero))
         (TFunc TUnit (TComp TNat (insert (opi 2) empty, toSymbol "_mu1")))
  )
  EUnit
t7 :: Type
t7 = CT $ TComp TNat (insert (opi 1) (insert (opi 2) empty), toSymbol "mu1")
e7 :: EnvC
e7 = initEnv sigma
