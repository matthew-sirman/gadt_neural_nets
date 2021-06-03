{-# LANGUAGE GADTs, DataKinds, KindSignatures, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module LinearAlgebra (
    Nat(..)
  , Vector(..)
  , Matrix(..)
  , Natural(..)
  , dot
  , transpose
  , matmul
) where

data Nat 
    = Z 
    | S Nat
    deriving Eq

data Vector (n :: Nat) a where
    VNil :: Vector Z a
    VCons :: a -> Vector l a -> Vector (S l) a

-- data Matrix n m a where
--     MNil :: Matrix Zero m a
--     MRow :: Vector m a -> Matrix r m a -> Matrix (Succ r) m a

instance Num (Vector Z a) where
    _ + _ = VNil
    _ - _ = VNil
    _ * _ = VNil
    abs _ = VNil
    signum _ = VNil
    fromInteger _ = VNil

instance (Num (Vector n a), Num a) => Num (Vector (S n) a) where
    (VCons a as) + (VCons b bs) = VCons (a + b) (as + bs)
    (VCons a as) - (VCons b bs) = VCons (a - b) (as - bs)
    (VCons a as) * (VCons b bs) = VCons (a * b) (as * bs)
    abs (VCons a as) = VCons (abs a) (abs as)
    signum (VCons a as) = VCons (signum a) (signum as)
    fromInteger n = VCons (fromInteger n) (fromInteger n)

instance Show (Vector Z a) where
    show _ = "[]"

instance (Show a, Show (Vector n a)) => Show (Vector (S n) a) where
    show (VCons a as) = show a ++ "," ++ show as

instance Functor (Vector n) where
    fmap _ VNil = VNil
    fmap f (VCons a as) = VCons (f a) (fmap f as)

-- class Populatable a where
--     populate :: b -> a b
-- 
-- instance Populatable (Vector Z) where
--     populate _ = VNil
-- 
-- instance Populatable (Vector n) => Populatable (Vector (S n)) where
--     populate elem = VCons elem $ populate elem

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

-- instance Num (Vector m a) => Num (Matrix Zero m a) where
--     _ + _ = MNil
--     _ - _ = MNil
--     _ * _ = MNil
--     abs _ = MNil
--     signum _ = MNil
--     fromInteger _ = MNil
-- 
-- instance (Num (Matrix n m a), Num (Vector m a)) => Num (Matrix (Succ n) m a) where
--     (MRow a as) + (MRow b bs) = MRow (a + b) (as + bs)
--     (MRow a as) - (MRow b bs) = MRow (a - b) (as - bs)
--     (MRow a as) * (MRow b bs) = MRow (a * b) (as * bs)
--     abs (MRow a as) = MRow (abs a) (abs as)
--     signum (MRow a as) = MRow (signum a) (signum as)
--     fromInteger n = MRow (fromInteger n) (fromInteger n)
-- 
-- instance Show (Matrix Zero m a) where
--     show _ = ""
-- 
-- instance (Show (Matrix n m a), Show (Vector m a)) => Show (Matrix (Succ n) m a) where
--     show (MRow r rs) = show r ++ "\n" ++ show rs
-- 
-- instance Functor (Vector m a) => Functor (Matrix Zero m a) where
--     fmap _ _ = MNil
-- 
-- instance (Functor (Matrix n m a), Functor (Vector m a)) => Functor (Matrix (Succ n) m a) where
--     fmap f (MRow r rs) = MRow (fmap f r) (fmap f rs)

type Matrix n m a = Vector n (Vector m a)

dot :: Num a => Vector n a -> Vector n a -> a
dot VNil VNil = 0
dot (VCons a as) (VCons b bs) = (a * b) + dot as bs

popRow :: Matrix (S n) m a -> (Matrix n m a, Vector m a)
popRow (VCons r rs) = (rs, r)

popCol :: Matrix n (S m) a -> (Matrix n m a, Vector n a)
popCol VNil = (VNil, VNil)
popCol (VCons (VCons a as) rs) = (VCons as mat, VCons a col)
    where
        (mat, col) = popCol rs

transpose :: (Natural m) => Matrix n m a -> Matrix m n a
transpose VNil = populate VNil
transpose (VCons r rs) = zipCons r $ transpose rs
    where
        zipCons :: Vector m a -> Matrix m n a -> Matrix m (S n) a
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

