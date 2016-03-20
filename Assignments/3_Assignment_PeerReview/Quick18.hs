{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}


module Quick where

import Data.Functor

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List as List
import Data.Maybe as Maybe

import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import System.Random (Random)

import Control.Applicative ((<|>))

------------------------------------------------------------------------------
-- QuickCheck properties for lists

prop_const' :: Eq a => a -> a -> Bool
prop_const' a b = const a b == a

-- *Main> quickCheck (prop_const :: Char -> Char -> Bool)

data Undefined
instance Testable Undefined where
  property = error "Unimplemented property"

prop_const :: Eq a => (a -> a -> a) -> a -> a -> Bool
prop_const const' a b = const' a b == a



constBug :: a -> a -> a
constBug _ b = b -- Oops: this returns the *second* argument, not the first.

prop_minimum :: Ord a => ([a] -> a) -> [a] -> Property
prop_minimum minimum' l =
  not (null l) ==> minimum l == minimum' l

minimumBug :: Ord a => [a] -> a
minimumBug = maximum

newtype SmallNonNeg a = SmallNonNeg a deriving (Eq, Ord, Show, Read)

instance (Num a, Random a, Arbitrary a) => Arbitrary (SmallNonNeg a) where
    arbitrary = SmallNonNeg <$> choose (0, 1000)
    shrink (SmallNonNeg x) = SmallNonNeg <$> shrink x

prop_replicate :: (Int -> a -> [a]) -> SmallNonNeg Int -> a -> Bool
prop_replicate replicate' (SmallNonNeg n) x =
    length (replicate n x) == length (replicate' n x)

replicateBug :: Int -> a -> [a]
replicateBug n = replicate (n + 1)

prop_group_1 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_1 group' l = concat (group' l) == l

prop_group_2 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_2 group' l = all ((== 1) . length . nub) (group' l)

groupBug :: Eq a => [a] -> [[a]]
groupBug []     = []
groupBug (x:xs) = ys : groupBug rest
    where (ys, rest) = span (==x) xs

prop_reverse_1 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_1 reverse' l = length l == length (reverse' l)

prop_reverse_2 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_2 reverse' l = reverse' (reverse' l) == l

reverseBug_1 :: [a] -> [a]
reverseBug_1 l = reverse (l ++ l)

reverseBug_2 :: Eq a => [a] -> [a]
reverseBug_2 [] = []
reverseBug_2 (_:xs) = reverse xs

listPropertiesMain :: IO ()
listPropertiesMain = do
  let qcName name prop = do
        putStr $ name ++ ": "
        quickCheck prop

  putStrLn "The following tests should all succeed:"
  qcName "const"     $ prop_const     (const     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimum   :: String -> Char)
  qcName "replicate" $ prop_replicate (replicate :: Int -> Char -> String)
  qcName "group_1"   $ prop_group_1   (group     :: String -> [String])
  qcName "group_2"   $ prop_group_2   (group     :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverse   :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverse   :: String -> String)

  putStrLn ""

  putStrLn "The following tests should all fail:"
  qcName "const"     $ prop_const     (constBug     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimumBug   :: String -> Char)
  qcName "replicate" $ prop_replicate (replicateBug :: Int -> Char -> String)
  qcName "group_1"   $ prop_group_1   (groupBug     :: String -> [String])
  qcName "group_2"   $ prop_group_2   (groupBug     :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverseBug_1 :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverseBug_2 :: String -> String)

------------------------------------------------------------------------------
-- Using QuickCheck to debug a SAT solver

---------------------------------------------------------------------------
-- Basic types

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses
type CNF = [ Clause ]

-- | A clause is a disjunction of a number of literals
data Clause = Clause [ Lit ] deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit Bool Var deriving (Eq, Ord, Show)

-- | A variable is just a character
data Var = Var Char
  deriving (Eq, Ord, Show)

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

-------------------------------------------------------------------------

-- | Extract the literals from a clause
lits :: Clause -> [Lit]
lits (Clause l) = l

-- | Extract the variable from a literal
var :: Lit -> Var
var (Lit _ x) = x

-- | Is the literal positive?
isPos :: Lit -> Bool
isPos (Lit b _) = b

-- | Determine the set of variables that appear in a formula
vars :: CNF -> Set Var
vars p = Set.unions $ map dVars p where
  dVars (Clause l) = Set.unions $ map (Set.singleton . var) l

instance Enum Var where
  toEnum i         = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

allVars :: [ Var ]
allVars = [vA .. ]

-------------------------------------------------------------------------

genVar      :: Int -> Gen Var
genVar    n = elements (take (abs n + 1) allVars)

genLit      :: Int -> Gen Lit
genLit    n = liftM2 Lit arbitrary (genVar n)

genClause   :: Int -> Gen Clause
genClause n = liftM Clause (listOf (genLit n))

genCNF      :: Int -> Gen CNF
genCNF    n = listOf (genClause n)

defaultNumVariables :: Int
defaultNumVariables = 5

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v | v == vA   = []
           | otherwise = [ vA .. pred v ]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) = map (flip Lit v) (shrink b) ++
                     map (Lit b) (shrink v)

instance Arbitrary Clause where
  arbitrary = genClause defaultNumVariables
  shrink (Clause l) = [Clause l' | l' <- shrink l]



---------------------------------------------------------------------
-- Satifiable and unsatisfiable formulae

exampleFormula :: CNF
exampleFormula = [Clause [Lit True vA, Lit True vB, Lit True vC],
                  Clause [Lit False vA],
                  Clause [Lit False vB, Lit True vC]]

unSatFormula :: CNF
unSatFormula = [Clause [Lit True vA],
                Clause [Lit False vA]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var,Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any (litSatisfied a) . lits) p

prop_satisfiedBy :: Bool
prop_satisfiedBy = exampleFormula `satisfiedBy` exampleValuation

extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert

value :: Var -> Valuation -> Maybe Bool
value = Map.lookup

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

makeValuations :: Set Var -> [Valuation]
makeValuations vs = map toSet $ subsequences elts
  where elts     = Set.toList vs
        toSet    = foldr (`extend` True) allFalse
        allFalse = Map.fromList $ zip elts $ repeat False

prop_makeValuations :: CNF -> Bool
prop_makeValuations p = length valuations == 2 ^ Set.size ss
                     && allElementsDistinct valuations where
   valuations = makeValuations ss
   ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = notElem x xs &&
                             allElementsDistinct xs

sat0 :: Solver
sat0 cnf = find (satisfiedBy cnf) vs
  where vs = makeValuations $ vars cnf

prop_satResultSound :: Solver -> Int -> Property
prop_satResultSound solver i =
  forAll (genCNF i) $ \p -> case solver p of
                               Just a  -> p `satisfiedBy` a
                               Nothing -> True

unsatisfiable :: CNF -> Bool
unsatisfiable p = all (\a -> not (p `satisfiedBy` a))
  (makeValuations (vars p))

prop_satResult :: Solver -> CNF -> Bool
prop_satResult solver p = case solver p of
                             Just a  -> p `satisfiedBy` a
                             Nothing -> unsatisfiable p

---------------------------------------------------------------------------
-- Instantiation

instantiate :: CNF -> Var -> Bool -> CNF
instantiate cnf v b = filter unsat $ map simplify cnf
  where unsat    = notElem (Lit b v) . lits
        simplify = Clause . filter (/= Lit (not b) v) . lits

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate cnf v = satisfiable == instantiable
  where satisfiable  = isJust $ sat0 cnf
        instantiable = isSatWith True || isSatWith False
        isSatWith = isJust . sat0 . instantiate cnf v

-- | Try instantiating a variable with True or False, and recurse on
--   the resulting valuation, if any. (Step 2 of sat1 instructions.)
tryInst :: Solver -> CNF -> Maybe Valuation
tryInst sat cnf = satWith True <|> satWith False
  where satWith b = extend aVar b <$> sat (instWith b)
        instWith  = instantiate cnf aVar
        aVar      = Set.elemAt 0 $ vars cnf

isSatisfied, isFalsified :: CNF -> Bool
isSatisfied = null
isFalsified = any null . map lits

sat1 :: Solver
sat1 = sat where
  sat cnf | isSatisfied cnf = Just emptyValuation
          | isFalsified cnf = Nothing
          | otherwise       = tryInst sat cnf

prop_sat1 :: CNF -> Bool
prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Unit propagation

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyUnitClause s) returns Nothing, then there
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s'), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause cnf =
  case simplifyUnitClause cnf of
    Just (cnf', _, _) -> unsatisfiable cnf' == unsatisfiable cnf
    Nothing           -> null $ unitClauses cnf

unitClauses :: CNF -> [Lit]
unitClauses cnf = concat $ onlyUnits $ map lits cnf
  where onlyUnits = filter ((== 1) . length)

simplifyUnitClause cnf =
  do (Lit b v) <- listToMaybe $ unitClauses cnf
     let cnf' = instantiate cnf v b
     return (cnf', v, b)

tryUnitClauses :: Solver -> CNF -> Maybe Valuation
tryUnitClauses sat cnf =
  case simplifyUnitClause cnf of
    Just (cnf', v, b) -> extend v b <$> sat cnf'
    Nothing           -> tryInst sat cnf

sat2 :: Solver
sat2 = sat where
  sat cnf | isSatisfied cnf = Just emptyValuation
          | isFalsified cnf = Nothing
          | otherwise       = tryUnitClauses sat cnf

prop_sat2 :: CNF -> Bool
prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Pure literal elimination

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyPureLiteral s) returns Nothing, then there
--    are no remaining pure literals in s
-- 2) If it returns (Just s'), then s' is satisfiable iff s is
prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral cnf =
  case simplifyPureLiteral cnf of
    Just (cnf', _, _) -> unsatisfiable cnf' == unsatisfiable cnf
    Nothing           -> null $ pureLiterals cnf

pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals cnf = posLits ++ negLits
  where posLits      = zip posVars $ repeat True
        negLits      = zip negVars $ repeat False
        posVars      = filter (`notElem` neg') pos'
        negVars      = filter (`notElem` pos') neg'
        (pos', neg') = (map var pos, map var neg)
        (pos, neg)   = partition isPos allLits
        allLits      = nub $ concatMap lits cnf

simplifyPureLiteral cnf =
  do (v, b) <- listToMaybe $ pureLiterals cnf
     let cnf' = instantiate cnf v b
     return (cnf', v, b)

tryUnitClauses' :: Solver -> CNF -> Maybe Valuation
tryUnitClauses' sat cnf =
  case simplifyUnitClause cnf of
    Just (cnf', v, b) -> extend v b <$> sat cnf'
    Nothing           -> tryPureLits sat cnf

tryPureLits :: Solver -> CNF -> Maybe Valuation
tryPureLits sat cnf =
  case simplifyPureLiteral cnf of
    Just (cnf', v, b) -> extend v b <$> sat cnf'
    Nothing           -> tryInst sat cnf

-- The final DPLL algorithm:
dpll :: Solver
dpll = sat where
  sat cnf | isSatisfied cnf = Just emptyValuation
          | isFalsified cnf = Nothing
          | otherwise       = tryUnitClauses' sat cnf

prop_dpll :: CNF -> Bool
prop_dpll s = isJust (dpll s) == isJust (sat0 s)

------------------------------------------------------------------------------
-- Using QC as a SAT solver

instance Arbitrary (Map Var Bool) where
  arbitrary = Map.fromList <$> arbitrary
  shrink v = Map.fromList <$> shrink (Map.toList v)

prop_isSatisfiable :: CNF -> Property
prop_isSatisfiable cnf = expectFailure (not . satisfiedBy cnf)

------------------------------------------------------------------------------
-- All the tests in one convenient place:

main :: IO ()
main = quickCheck $    prop_satisfiedBy
                  .&&. prop_satResultSound sat0 defaultNumVariables
                  .&&. prop_satResult      sat0
                  .&&. prop_instantiate
                  .&&. prop_sat1
                  .&&. prop_satResultSound sat1
                  .&&. prop_satResult      sat1
                  .&&. prop_simplifyUnitClause
                  .&&. prop_sat2
                  .&&. prop_satResultSound sat2
                  .&&. prop_satResult      sat2
                  .&&. prop_simplifyPureLiteral
                  .&&. prop_dpll
                  .&&. prop_satResultSound dpll
                  .&&. prop_satResult      dpll
