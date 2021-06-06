{-# LANGUAGE FlexibleInstances, KindSignatures, TypeFamilies, TypeOperators, RankNTypes, InstanceSigs, MultiParamTypeClasses #-}

module Differentation (
    d
) where

import LinearAlgebra

data Dual a = Dual
    { runEval :: a
    , runDeriv :: a
    }
    deriving (Show, Eq)

instance Num a => Num (Dual a) where
    (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
    (Dual u u') - (Dual v v') = Dual (u - v) (u' - v')
    (Dual u u') * (Dual v v') = Dual (u * v) (u' * v + u * v')
    abs (Dual u u') = Dual (abs u) (u * signum u)
    signum (Dual u u') = Dual (signum u) 0
    fromInteger n = Dual (fromInteger n) 0

instance Fractional a => Fractional (Dual a) where
    (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / (v * v))
    fromRational n = Dual (fromRational n) 0

instance (Floating a, Eq a) => Floating (Dual a) where
    pi = Dual pi 0
    exp (Dual u u') = Dual (exp u) (u' * exp u)
    log (Dual u u') = Dual (log u) (u' / u)
    sqrt (Dual u u') = Dual (sqrt u) (u' / (2 * sqrt u))
    sin (Dual u u') = Dual (sin u) (u' * (cos u))
    cos (Dual u u') = Dual (cos u) (-1 * u' * (sin u))
    tan (Dual u u') = Dual (tan u) (u' / ((cos u) ** 2))
    asin (Dual u u') = Dual (acos u) (u' / (sqrt (1 - (u ** 2))))
    acos (Dual u u') = Dual (acos u) (-1 * u' / (sqrt (1 - (u ** 2))))
    atan (Dual u u') = Dual (atan u) (u' / (1 + (u ** 2)))
    sinh (Dual u u') = Dual (sinh u) (u' * cosh u)
    cosh (Dual u u') = Dual (cosh u) (u' * sinh u)
    tanh (Dual u u') = Dual (tanh u) (u' * (1 - ((tanh u) ** 2)))
    asinh (Dual u u') = Dual (asinh u) (u' / (sqrt (1 + (u ** 2))))
    acosh (Dual u u') = Dual (acosh u) (u' / (sqrt ((u ** 2) - 1)))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - (u ** 2)))
    (Dual u u') ** (Dual n 0) = Dual (u ** n) (u' * n * u ** (n - 1))
    (Dual a 0) ** (Dual v v') = Dual (a ** v) (v' * log a * a ** v)
    (Dual u u') ** (Dual v v') = Dual (u ** v) ((u ** v) * (v' * (log u) + (v * u' / u)))
    logBase (Dual u u') (Dual v v') = Dual (logBase u v) (((log v) * u' / u - (log u) * v' / v) / ((log u) ** 2))

instance Ord a => Ord (Dual a) where
    (Dual u _) <= (Dual v _) = u <= v

-- instance VectorType v => VectorType (Dual :. v) where
--     dot (O (Dual u u')) (O (Dual v v')) = undefined

-- instance VectorType Dual Dual where
--     dot :: Dual (v a) -> Dual (v a) -> Dual a
--     dot (Dual u u') (Dual v v') = Dual (dot u v) 1

-- instance VectorType Identity where
--     dot :: (Num a) => Identity a -> Identity a -> a
--     dot (Identity x) (Identity y) = x * y

d :: (Num a) => (Dual a -> Dual b) -> a -> b
d f x = runDeriv $ f (Dual x 1)

