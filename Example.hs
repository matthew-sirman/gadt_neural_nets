{-# LANGUAGE GADTs, DataKinds #-}

module Example where

import NeuralNet
import System.Random

type L1 = DenseLayer (ToNat 2) (ToNat 5) Float
type L2 = DenseLayer (ToNat 5) (ToNat 1) Float

type Model = DenseNetwork (ToNat 2) (ToNat 1) '[ToNat 5] Float

initModel :: (Float, Float) -> StdGen -> Model
initModel range gen = DNCons l1 (DNOne l2)    
    where
        l1 :: L1
        l1 = mkRandomDense range gen relu

        l2 :: L2
        l2 = mkRandomDense range gen sigmoid

type Samples = 100

genParitySamples :: StdGen -> (Matrix (ToNat 2) (ToNat Samples) Float, Matrix (ToNat 1) (ToNat Samples) Float)
genParitySamples gen = (transpose $ featureMatrix features (populate (populate 0)), 
                        transpose $ labelMatrix labels (populate (populate 0)))
    where
        featureMatrix :: [(Float, Float)] -> Matrix s (ToNat 2) Float -> Matrix s (ToNat 2) Float
        featureMatrix _ VNil = VNil
        featureMatrix ((x, y):xys) (VCons _ rest) = VCons (VCons x (VCons y VNil)) (featureMatrix xys rest)

        labelMatrix :: [Float] -> Matrix s (ToNat 1) Float -> Matrix s (ToNat 1) Float
        labelMatrix _ VNil = VNil
        labelMatrix (l:ls) (VCons _ rest) = VCons (VCons l VNil) (labelMatrix ls rest)

        points :: [Int]
        points = randomRs (0, 3) gen

        inputSamples :: [(Float, Float)]
        inputSamples = mkPairs stream
            where
                stream :: [Float]
                stream = randomRs (-0.2, 0.2) gen

                mkPairs :: [Float] -> [(Float, Float)]
                mkPairs (x:y:xys) = (x, y) : mkPairs xys

        features :: [(Float, Float)]
        features = features' points inputSamples
            where
                features' :: [Int] -> [(Float, Float)] -> [(Float, Float)]
                features' (p:ps) ((x, y):xys)
                    | p == 0 = (x, y) : features' ps xys
                    | p == 1 = (x + 1, y) : features' ps xys
                    | p == 2 = (x, y + 1) : features' ps xys
                    | p == 3 = (x + 1, y + 1) : features' ps xys

        labels :: [Float]
        labels = labels' points
            where
                labels' :: [Int] -> [Float]
                labels' (p:ps)
                    | p == 0 = 0 : labels' ps
                    | p == 1 = 1 : labels' ps
                    | p == 2 = 1 : labels' ps
                    | p == 3 = 0 : labels' ps

