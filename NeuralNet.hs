{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, ScopedTypeVariables, FlexibleContexts, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module NeuralNet where

import LinearAlgebra
import Differentation
import Data.Kind (Constraint)

type family All (c :: Nat -> Constraint) (ls :: [Nat]) :: Constraint where
    All _ '[] = Natural Z
    All c (x ': xs) = (c x, All c xs)

data DenseLayer (i :: Nat) (o :: Nat) a
    = DenseLayer (Matrix o i a) (Vector o a)

data DenseNetwork (iSize :: Nat) (oSize :: Nat) (ls :: [Nat]) w where
    DNOne :: DenseLayer i o w -> DenseNetwork i o '[] w
    DNCons :: DenseLayer i l w -> DenseNetwork l o ls w -> DenseNetwork i o (l : ls) w

runDense :: forall i o a b. (Num a, Natural o, Natural b) 
         => DenseLayer i o a -> Matrix i b a -> Matrix o b a
runDense (DenseLayer weights biases) input = addBiases biases (matmul weights input)
    where
        addBiases :: Vector n a -> Matrix n b a -> Matrix n b a
        addBiases VNil VNil = VNil
        addBiases (VCons b bs) (VCons r rs) = VCons ((+b) <$> r) (addBiases bs rs)

runNetwork :: (Num a, Natural o, Natural b, All Natural ls)
           => DenseNetwork i o ls a -> Matrix i b a -> Matrix o b a
runNetwork (DNOne layer) inputs = runDense layer inputs
runNetwork (DNCons layer rest) inputs = runNetwork rest (runDense layer inputs)

