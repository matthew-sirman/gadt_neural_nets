{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, ScopedTypeVariables, FlexibleContexts, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, RankNTypes #-}

module NeuralNet (
    DenseLayer(..)
  , DenseNetwork(..)
  , runNetwork
  , trainNetwork
  , mkRandomDense
  , populateVector
  , populateMatrix
  , relu
  , linear
  , sigmoid
  , binaryCrossEntropy
  , binaryCrossEntropyDeriv
  , module LinearAlgebra
  , module Differentation
) where

import LinearAlgebra
import Differentation
import Data.Kind (Constraint)
import System.Random

type family All (c :: Nat -> Constraint) (ls :: [Nat]) :: Constraint where
    All _ '[] = Natural Z
    All c (x ': xs) = (c x, All c xs)

data DenseLayer (i :: Nat) (o :: Nat) a = DenseLayer 
    { weights :: (Matrix o i a) 
    , biases :: (Vector o a)
    , activation :: (forall b. (Floating b, Ord b) => b -> b)
    }

data DenseNetwork (iSize :: Nat) (oSize :: Nat) (ls :: [Nat]) w where
    DNOne :: DenseLayer i o w -> DenseNetwork i o '[] w
    DNCons :: DenseLayer i l w -> DenseNetwork l o ls w -> DenseNetwork i o (l : ls) w

mkRandomDense :: (Random a, Num a, Natural o, Natural i) 
              => (a, a) -> StdGen -> (forall b. (Floating b, Ord b) => b -> b) -> DenseLayer i o a
mkRandomDense range gen activation = DenseLayer (populateMatrix range gen) (populateVector range gen) activation

runDense :: forall i o a b. (Floating a, Eq a, Ord a, Natural o, Natural b) 
         => DenseLayer i o a -> Matrix i b a -> Matrix o b a
runDense layer input = snd $ runDense' layer input

runDense' :: forall i o a b. (Floating a, Eq a, Ord a, Natural o, Natural b)
          => DenseLayer i o a -> Matrix i b a -> (Matrix o b a, Matrix o b a)
runDense' (DenseLayer weights biases activation) input = (aOut, zOut)
    where
        aOut = addBiases biases (matmul weights input)
        zOut = fmap (fmap activation) aOut

        addBiases :: Vector n a -> Matrix n b a -> Matrix n b a
        addBiases VNil VNil = VNil
        addBiases (VCons b bs) (VCons r rs) = VCons ((+b) <$> r) (addBiases bs rs)

runNetwork :: (Floating a, Eq a, Ord a, Natural o, Natural b, All Natural ls)
           => DenseNetwork i o ls a -> Matrix i b a -> Matrix o b a
runNetwork (DNOne layer) inputs = runDense layer inputs
runNetwork (DNCons layer rest) inputs = runNetwork rest (runDense layer inputs)

trainNetwork :: forall a i o b ls. (Floating a, Eq a, Ord a, Natural i, Natural o, Natural b, All Natural ls)
             => Int
             -> a
             -> (Vector o a -> Vector o a -> a)
             -> (Vector o a -> Vector o a -> Vector o a)
             -> Matrix o b a
             -> DenseNetwork i o ls a
             -> Matrix i b a
             -> DenseNetwork i o ls a
trainNetwork 0 _ _ _ _ network _ = network
trainNetwork epoch eta loss loss' labels network inputs = 
    trainNetwork (epoch - 1) eta loss loss' labels (update network) inputs
    where
        update net = trainNetworkStep eta loss loss' labels net inputs

trainNetworkStep :: forall a i o b ls. (Floating a, Eq a, Ord a, Natural i, Natural o, Natural b, All Natural ls)
                 => a
                 -> (Vector o a -> Vector o a -> a)
                 -> (Vector o a -> Vector o a -> Vector o a)
                 -> Matrix o b a
                 -> DenseNetwork i o ls a
                 -> Matrix i b a
                 -> DenseNetwork i o ls a
trainNetworkStep eta loss loss' labels network inputs = fst $ trainNetwork' network inputs
    where
        trainNetwork' :: (Natural i', All Natural ls')
                      => DenseNetwork i' o ls' a
                      -> Matrix i' b a
                      -> (DenseNetwork i' o ls' a, Matrix i' b a)
        trainNetwork' (DNOne layer) inputs =
            let (as, zs) = runDense' layer inputs
                dldz = calcLossDeriv zs
                dzda = fmap (fmap $ d $ activation layer) as
                dw = fmap (fmap (/batchSize)) $ matmul (dldz * dzda) (transpose inputs)
                db = fmap ((/batchSize) . foldr (+) 0) (dldz * dzda)
                din = matmul (transpose $ weights layer) (dldz * dzda) in
            (DNOne (layer 
                    { weights = weights layer - (fmap (fmap (*eta)) dw)
                    , biases = biases layer - (fmap (*eta) db)
                    })
                , din)
        trainNetwork' (DNCons layer network) inputs =
            let (as, zs) = runDense' layer inputs
                (rest, dldz) = trainNetwork' network zs
                dzda = fmap (fmap $ d $ activation layer) as
                dw = fmap (fmap (/batchSize)) $ matmul (dldz * dzda) (transpose inputs)
                db = fmap ((/batchSize) . foldr (+) 0) (dldz * dzda)
                din = matmul (transpose $ weights layer) (dldz * dzda) in
            (DNCons (layer
                    { weights = weights layer - (fmap (fmap (*eta)) dw)
                    , biases = biases layer - (fmap (*eta) db)
                    }) rest
                , din)

        batchSize :: Fractional b' => b'
        batchSize = foldr (const (+1)) 0 $ batchLengthVec
            where
                batchLengthVec :: Vector b a
                batchLengthVec = populate 0

        calcLoss :: Matrix o b a -> a
        calcLoss outputs = calcLoss' (transpose labels) (transpose outputs)
            where
                calcLoss' :: Matrix b' o a -> Matrix b' o a -> a
                calcLoss' VNil VNil = 0
                calcLoss' (VCons y ys) (VCons o os) = loss y o + calcLoss' ys os

        calcLossDeriv :: Matrix o b a -> Matrix o b a
        calcLossDeriv outputs = transpose $ calcLossDeriv' (transpose labels) (transpose outputs)
            where
                calcLossDeriv' :: Matrix b' o a -> Matrix b' o a -> Matrix b' o a
                calcLossDeriv' VNil VNil = VNil
                calcLossDeriv' (VCons y ys) (VCons o os) = VCons (loss' y o) (calcLossDeriv' ys os)

populateVector :: forall a n. (Random a, Num a, Natural n) => (a, a) -> StdGen -> Vector n a
populateVector range gen = fillRow (randomRs range gen) $ populate 0
    where
        fillRow :: [a] -> Vector n' a -> Vector n' a
        fillRow _ VNil = VNil
        fillRow (r:rs) (VCons _ as) = VCons r (fillRow rs as)

populateMatrix :: forall n m a. (Random a, Num a, Natural n, Natural m) => (a, a) -> StdGen -> Matrix n m a
populateMatrix range gen = randomise (randomRs range gen) $ populate (populate 0)
    where
        randomise :: [a] -> Matrix n' m a -> Matrix n' m a
        randomise _ VNil = VNil
        randomise rands (VCons r rs) = 
            let (row, restRands) = fillRow rands r in
            VCons row (randomise restRands rs)

        fillRow :: [a] -> Vector m' a -> (Vector m' a, [a])
        fillRow rands VNil = (VNil, rands)
        fillRow (r:rands) (VCons _ as) =
            let (restVec, restRands) = fillRow rands as in
            (VCons r restVec, restRands)

-- Activation Functions

relu :: (Num a, Ord a) => a -> a
relu x
    | x < 0 = 0
    | otherwise = x

linear :: (Num a) => a -> a
linear = id

sigmoid :: (Floating a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

binaryCrossEntropy :: Floating a => Vector (ToNat 1) a -> Vector (ToNat 1) a -> a
binaryCrossEntropy (VCons y _) (VCons o _) = y * log o + (1 - y) * log (1 - o)

binaryCrossEntropyDeriv :: Floating a => Vector (ToNat 1) a -> Vector (ToNat 1) a -> Vector (ToNat 1) a
binaryCrossEntropyDeriv (VCons y _) (VCons o _) = VCons (y / o + (1 - y) / (1 - o)) VNil

