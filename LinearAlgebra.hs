{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, InstanceSigs, MultiParamTypeClasses, TypeFamilies, TypeOperators, UndecidableInstances #-}

module LinearAlgebra (
    Nat(..)
  , ToNat
  , Vector(..)
  , Matrix(..)
  , Natural(..)
  -- , VectorType(..)
  -- , MatrixType(..)
  , dot
  , matmul
  , transpose
  , populate
) where

import qualified GHC.Types as T -- (Nat)
import qualified GHC.TypeLits as TL -- ((-))

data Nat 
    = Z 
    | S Nat
    deriving Eq

type family ToNat (n :: T.Nat) :: Nat where
    ToNat 0 = Z
    ToNat n = S (ToNat (n TL.- 1))

data Vector (n :: Nat) a where
    VNil :: Vector Z a
    VCons :: a -> Vector l a -> Vector (S l) a

-- instance Num (Vector Z a) where
--     _ + _ = VNil
--     _ - _ = VNil
--     _ * _ = VNil
--     abs _ = VNil
--     signum _ = VNil
--     fromInteger _ = VNil
-- 
-- instance (Num (Vector n a), Num a) => Num (Vector (S n) a) where
--     (VCons a as) + (VCons b bs) = VCons (a + b) (as + bs)
--     (VCons a as) - (VCons b bs) = VCons (a - b) (as - bs)
--     (VCons a as) * (VCons b bs) = VCons (a * b) (as * bs)
--     abs (VCons a as) = VCons (abs a) (abs as)
--     signum (VCons a as) = VCons (signum a) (signum as)
--     fromInteger n = VCons (fromInteger n) (fromInteger n)

vectorZipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vectorZipWith f VNil VNil = VNil
vectorZipWith f (VCons a as) (VCons b bs) = VCons (f a b) (vectorZipWith f as bs)

instance (Natural n, Num a) => Num (Vector n a) where
    (+) = vectorZipWith (+)
    (-) = vectorZipWith (-)
    (*) = vectorZipWith (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = populate . fromInteger

instance Show (Vector Z a) where
    show _ = "[]"

instance (Show a, Show (Vector n a)) => Show (Vector (S n) a) where
    show (VCons a as) = show a ++ "," ++ show as

instance Functor (Vector n) where
    fmap _ VNil = VNil
    fmap f (VCons a as) = VCons (f a) (fmap f as)

instance Foldable (Vector n) where
    foldr _ e VNil = e
    foldr f e (VCons a as) = foldr f (f a e) as

class Natural n where
    natCase :: NatCase g => g n

class NatCase g where
    caseZero :: g Z
    caseSucc :: Natural n => g (S n)

instance Natural Z where
    natCase = caseZero

instance Natural n => Natural (S n) where
    natCase = caseSucc

newtype MkVec a n = MkVec { runMkVec :: a -> Vector n a }

populate :: Natural n => a -> Vector n a
populate = runMkVec natCase

instance NatCase (MkVec a) where
    caseZero = MkVec (\x -> VNil)
    caseSucc = MkVec (\x -> VCons x (populate x))

-- newtype Matrix n m a = Matrix (Vector n (Vector m a))
type Matrix n m a = Vector n (Vector m a)

-- class Functor v => VectorType v where
--     dot :: Num a => v a -> v a -> a
-- 
-- class MatrixType (mat :: Nat -> Nat -> * -> *) where
--     transpose :: (Natural m) => mat n m a -> mat m n a
--     matmul :: (Num a, Natural m) => mat n k a -> mat k m a -> mat n m a
-- 
-- instance VectorType (Vector n) where
--     dot VNil VNil = 0
--     dot (VCons a as) (VCons b bs) = (a * b) + dot as bs
-- 
-- instance MatrixType Matrix where
--     transpose :: (Natural m) => Matrix n m a -> Matrix m n a
--     transpose (Matrix VNil) = Matrix $ populate VNil
--     transpose (Matrix (VCons r rs)) = zipCons r $ transpose $ Matrix rs
--         where
--             zipCons :: Vector m a -> Matrix m n a -> Matrix m (S n) a
--             zipCons (VCons a as) (Matrix (VCons c cs)) = 
--                 let Matrix rest = zipCons as (Matrix cs) in
--                 Matrix $ VCons (VCons a c) rest
-- 
--     matmul :: forall n m k a. (Num a, Natural m) => Matrix n k a -> Matrix k m a -> Matrix n m a
--     matmul m1 m2 = matmul' m1
--         where
--             m2t :: Vector m (Vector k a)
--             (Matrix m2t) = transpose m2
--         
--             -- matmul' :: (Populatable (Vector n)) => Matrix n' k a -> Matrix n' m a
--             matmul' :: Matrix n' k a -> Matrix n' m a
--             matmul' (Matrix VNil) = Matrix VNil
--             matmul' (Matrix (VCons a as)) = 
--                 let Matrix rest = matmul' (Matrix as) in
--                 Matrix $ VCons (dot a <$> m2t) rest
-- 
-- popRow :: Matrix (S n) m a -> (Matrix n m a, Vector m a)
-- popRow (VCons r rs) = (rs, r)
-- 
-- popCol :: Matrix n (S m) a -> (Matrix n m a, Vector n a)
-- popCol VNil = (VNil, VNil)
-- popCol (VCons (VCons a as) rs) = (VCons as mat, VCons a col)
--     where
--         (mat, col) = popCol rs

dot :: (Num a) => Vector n a -> Vector n a -> a
dot VNil VNil = 0
dot (VCons a as) (VCons b bs) = (a * b) + dot as bs

transpose :: (Natural m) => Matrix n m a -> Matrix m n a
transpose VNil = populate VNil
transpose (VCons r rs) = zipCons r $ transpose rs
    where
        zipCons :: Vector m a -> Matrix m n a -> Matrix m (S n) a
        zipCons VNil VNil = VNil
        zipCons (VCons a as) (VCons c cs) = VCons (VCons a c) (zipCons as cs)

matmul :: forall n m k a. (Num a, Natural m) => Matrix n k a -> Matrix k m a -> Matrix n m a
matmul m1 m2 = matmul' m1
    where
        m2t :: Matrix m k a
        m2t = transpose m2
    
        -- matmul' :: (Populatable (Vector n)) => Matrix n' k a -> Matrix n' m a
        matmul' :: Matrix n' k a -> Matrix n' m a
        matmul' VNil = VNil
        matmul' (VCons a as) = VCons (dot a <$> m2t) (matmul' as)

