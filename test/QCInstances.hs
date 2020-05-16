module QCInstances where

import           Test.QuickCheck
import           AST
import           Types
import           Data.Either
import           Utils.Symbol
import qualified Utils.Substitution            as Sub
import qualified Utils.Set                     as S
import           Printing.PPTypes
import           Printing.PPAST
import           Common
import           Operation
import           EffectRow
import           TypeEnv
import           Data.Maybe                     ( fromJust )

-- | Arbitrary instance for Value Types
instance Arbitrary VType where
  arbitrary = sized $ arbitrarySizedVType (fst <$> sigma)

-- | Arbitrary instance for Computation Types
instance Arbitrary CType where
  arbitrary = sized $ arbitrarySizedCType (fst <$> sigma)

-- | Arbitrary instance for Types
instance Arbitrary Type where
  arbitrary = oneof [VT <$> arbitrary, CT <$> arbitrary]

-- | Associated pair of terms and their types 
data TypedTerm = TT Term Type

instance Show TypedTerm where
  show (TT term typ) =
    "Type: \n" ++ renderType typ ++ "\n\nTerm: \n" ++ renderTerm term

-- | Arbitrary instance for TypedTerms. We first generate an arbitrary type
-- and then construct an arbitrary term of that type.
instance Arbitrary TypedTerm where
  arbitrary = do
    typ <- arbitrary
    case typ of
      VT vt -> do
        e <- sized $ arbitrarySizedExp (initEnv sigma) vt
        return $ TT (E e) typ
      CT ct -> do
        c <- sized $ arbitrarySizedComp (initEnv sigma) ct
        return $ TT (C c) (CT ct)


arbitrarySizedVType :: [Operation] -> Int -> Gen VType
arbitrarySizedVType sigma 0  = elements [TBool, TNat, TUnit]
arbitrarySizedVType sigma sz = frequency
  [ (1, return TBool)
  , (1, return TNat)
  , (1, return TUnit)
  , (0, return TEmpty)
  , ( 1
    , do
      vt <- arbitrarySizedVType sigma (div sz 2)
      TFunc vt <$> arbitrarySizedCType sigma (div sz 2)
    )
  , ( 1
    , do
      mu <- arbitraryVar "mu"
      ct <- arbitrarySizedCTypeEVar sigma (div sz 2) mu
      THand ct <$> arbitrarySizedCTypeEVar sigma (div sz 2) mu
    )
  ]

arbitrarySizedCType :: [Operation] -> Int -> Gen CType
arbitrarySizedCType sigma sz = do
  vt <- arbitrarySizedVType sigma (div sz 2)
  TComp vt <$> arbitraryEffRow sigma

arbitrarySizedCTypeEVar :: [Operation] -> Int -> EffVar -> Gen CType
arbitrarySizedCTypeEVar sigma sz mu = do
  vt <- arbitrarySizedVType sigma (div sz 2)
  TComp vt <$> arbitraryEffRowEVar sigma mu

arbitraryEffRow :: [Operation] -> Gen EffRow
arbitraryEffRow sigma = do
  ev <- arbitraryVar "mu"
  arbitraryEffRowEVar sigma ev

arbitraryEffRowEVar :: [Operation] -> EffVar -> Gen EffRow
arbitraryEffRowEVar sigma mu = do
  ops <- someof sigma
  return $ foldr addOp (emptyRow mu) ops

arbitrarySizedExp :: TypeEnv e => e -> VType -> Int -> Gen Exp
arbitrarySizedExp te TBool _ =
  let nvars = nVarsInCtx te TBool
  in  frequency
        [ (1    , return ETrue)
        , (1    , return EFalse)
        , (nvars, arbitraryVarInCtx te TBool)
        ]
arbitrarySizedExp te TUnit _ =
  let nvars = nVarsInCtx te TUnit
  in  frequency [(1, return EUnit), (nvars, arbitraryVarInCtx te TUnit)]
arbitrarySizedExp te TNat sz =
  let nvars   = length $ filter ((== VT TNat) . snd) (vars te)
      sizeLim = if sz == 0 then 0 else 1
  in  frequency
        [ (1      , return EZero)
        , (sizeLim, ESucc <$> arbitrarySizedExp te TNat (div sz 2))
        , (nvars  , arbitraryVarInCtx te TNat)
        ]
arbitrarySizedExp te t@(TFunc a c) sz =
  let nvars   = nVarsInCtx te t
      sizeLim = if sz == 0 && nvars /= 0 then 0 else 1
  in  frequency
        [ ( sizeLim
          , do
            x  <- arbitraryVar "x"
            bd <- arbitrarySizedComp (extEnv x (VT a) te) c (div sz 2)
            t' <- randomRename t
            return $ EAnno (EFunc x bd) t'
          )
        , (nvars, arbitraryVarInCtx te t)
        ]
arbitrarySizedExp te t@(THand ct@(TComp c er) dt@(TComp d er')) sz =
  let nvars   = nVarsInCtx te t
      sizeLim = if sz == 0 && nvars /= 0 then 0 else 1
  in  frequency
        [ ( sizeLim
          , do
            let opers = operations er
            x       <- arbitraryVar "x"
            cv      <- arbitrarySizedComp (extEnv x (VT c) te) dt (div sz 2)
            clauses <- mapM
              (\opi -> do
                xi <- arbitraryVar "x"
                ki <- arbitraryVar "k"
                let (TOp ai bi) = fromJust $ lookup opi (ops te)
                ci <- arbitrarySizedComp
                  (extEnv xi (VT ai) (extEnv ki (VT $ TFunc bi dt) te))
                  dt
                  (div sz 4)
                return (opi, xi, ki, ci)
              )
              opers
            t' <- randomRename t
            return $ EAnno (EHand x cv clauses) t'
          )
        , (nvars, arbitraryVarInCtx te t)
        ]

randomRename :: VType -> Gen VType
randomRename t = do
  let effvars = fv (VT t)
  newVars <- mapM (\_ -> arbitraryVar "mu") effvars
  let sub = Sub.unlist $ zip effvars (emptyRow <$> newVars)
  return (Sub.apply sub t)

arbitraryVar :: String -> Gen Var
arbitraryVar prefix = do
  n <- choose (0, 999999) :: Gen Int
  return $ toSymbol (prefix ++ show n)

arbitraryVarInCtx :: TypeEnv e => e -> VType -> Gen Exp
arbitraryVarInCtx te t =
  let tvars = fst <$> filter ((== VT t) . snd) (vars te)
  in  EVar <$> elements tvars

nVarsInCtx :: TypeEnv e => e -> VType -> Int
nVarsInCtx te t = length $ filter ((== VT t) . snd) (vars te)

arbitrarySizedComp :: TypeEnv e => e -> CType -> Int -> Gen Comp
arbitrarySizedComp te ct@(TComp a er) 0 = CVal <$> arbitrarySizedExp te a 0
arbitrarySizedComp te ct@(TComp a er) sz =
  let nOps = length (operations er)
  in  frequency
        [ (1, CVal <$> arbitrarySizedExp te a (div sz 2))
        , ( nOps
          , do
            op <- elements (operations er)
            let (TOp ai bi) = fromJust $ lookup op (ops te)
            e <- arbitrarySizedExp te ai (div sz 2)
            y <- arbitraryVar "y"
            c <- arbitrarySizedComp (extEnv y (VT bi) te) ct (div sz 2)
            return $ COp op e y c
          )
        , ( 1
          , do
            mu            <- arbitraryVar "mu"
            handled       <- someof (fst <$> ops te)
            (TComp at ar) <- arbitrarySizedCTypeEVar handled (div sz 2) mu
            produced      <- someof (operations er)
            let bt = TComp a (foldr addOp (emptyRow mu) produced)
            h   <- arbitrarySizedExp te (THand (TComp at ar) bt) (div sz 2)
            er' <- arbitraryEffRow (operations er)
            c   <- arbitrarySizedComp te (TComp at er') (div sz 2)
            return $ CWith h c
          )
        , ( 1
          , do
            a   <- arbitrarySizedVType (operations er) (div sz 2)
            ct' <- smallerCType ct
            e1  <- arbitrarySizedExp te (TFunc a ct') (div sz 2)
            e2  <- arbitrarySizedExp te a (div sz 2)
            return $ CApp e1 e2
          )
        , ( 1
          , do
            e    <- arbitrarySizedExp te TBool (div sz 2)
            ct'  <- smallerCType ct
            ct'' <- smallerCType ct
            c1   <- arbitrarySizedComp te ct' (div sz 2)
            c2   <- arbitrarySizedComp te ct'' (div sz 2)
            return $ CIf e c1 c2
          )
        , ( 1
          , do
            e    <- arbitrarySizedExp te TNat (div sz 2)
            ct'  <- smallerCType ct
            ct'' <- smallerCType ct
            c1   <- arbitrarySizedComp te ct' (div sz 2)
            x    <- arbitraryVar "x"
            c2   <- arbitrarySizedComp (extEnv x (VT TNat) te) ct'' (div sz 2)
            return $ CMatch e c1 x c2
          )
        , ( 1
          , do
            x            <- arbitraryVar "x"
            (TComp a ar) <- arbitrarySizedCType (operations er) (div sz 2)
            c1           <- arbitrarySizedComp te (TComp a ar) (div sz 2)
            ct'          <- smallerCType ct
            c2 <- arbitrarySizedComp (extEnv x (VT a) te) ct' (div sz 2)
            return $ CLet x c1 c2
          )
        ]


smallerCType :: CType -> Gen CType
smallerCType (TComp a (d, m)) = do
  d' <- S.fromList <$> someof (S.toList d)
  return $ TComp a (d', m)

someof :: [a] -> Gen [a]
someof l = do
  n  <- choose (1, length l)
  l' <- shuffle l
  return (take n l')

