{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad
import           Control.Monad.ST
import           Criterion.Main
import           Criterion.Types
import qualified Data.List as L
import           Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Storable as SV
import           System.Directory
import           System.Random

import qualified Int.FingerTree as UF
import qualified HaskellWorks.Data.FingerTree as F

data Conser = forall fInt. NFData fInt => Conser String (Int -> IO fInt) (Int -> fInt -> fInt)
data Append = forall fInt. NFData fInt => Append String (Int -> IO fInt) (fInt -> fInt -> fInt) (fInt -> fInt)
data Replicator = forall fInt. NFData fInt => Replicator String (Int -> Int -> fInt)
data Indexing = forall fInt. NFData fInt => Indexing String (IO fInt) (fInt -> Int -> Int)
data Length = forall fInt. NFData fInt => Length String (Int -> IO fInt) (fInt -> Int)
data Min = forall fInt. NFData fInt => Min String (Int -> IO fInt) (fInt -> Int)
data Max = forall fInt. NFData fInt => Max String (Int -> IO fInt) (fInt -> Int)
data Sort = forall fInt. NFData fInt => Sort String (Int -> IO fInt) (fInt -> fInt)
data RemoveElement = forall fInt. NFData fInt => RemoveElement String (IO fInt) ((Int -> Bool) -> fInt -> fInt)
data RemoveByIndex = forall fInt. NFData fInt => RemoveByIndex String (IO fInt) ((Int -> Int -> Bool) -> fInt -> fInt)

main :: IO ()
main = do
  let fp = "out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  defaultMainWith
    defaultConfig {csvFile = Just fp}
    {-
    [ bgroup
        "Consing"
        (conses
           [ Conser "Data.List" sampleList (:)
           --, Conser "Data.Vector" sampleVector V.cons
           --, Conser "Data.Vector.Unboxed" sampleUVVector UV.cons
           --, Conser "Data.Vector.Storable" sampleSVVector SV.cons
           , Conser "Data.Sequence" sampleSeq (S.<|)
           , Conser "UnpackedFingerTree" sampleUnpackedFingerTree (UF.<|)
           , Conser "FingerTree" sampleFingerTree (F.<|)
           ])
    -}
    [ bgroup
        "Indexing"
        (let size = 10005
         in indexes
           --   [ Indexing "Data.List" (sampleList size) (L.!!)
           --   , Indexing "Data.Vector" (sampleVector size) (V.!)
           --   , Indexing "Data.Vector.Unboxed" (sampleUVVector size) (UV.!)
           --   , Indexing "Data.Vector.Storable" (sampleSVVector size) (SV.!)
              [ Indexing "Data.Sequence" (sampleSeq size) (S.index)
              , Indexing "UnpackedFingerTree" (sampleUnpackedFingerTree size) uftIndex
           --   , Indexing "FingerTree" (sampleFingerTree size) ftIndex
              ])
    , bgroup
        "Append"
        (appends
           [ Append "Data.List" sampleList (<>) force
           --, Append "Data.Vector" sampleVector (<>) id
           --, Append "Data.Vector.Unboxed" sampleUVVector (<>) id
           --, Append "Data.Vector.Storable" sampleSVVector (<>) id
           , Append "Data.Sequence" sampleSeq (<>) id
           , Append "UnpackedFingerTree" sampleUnpackedFingerTree (<>) id
           , Append "FingerTree" sampleFingerTree (<>) id
           ])
    , bgroup
        "Length"
        (lengths
           [ Length "Data.List" sampleList (L.length)
           , Length "Data.Vector" sampleVector (V.length)
           , Length "Data.Vector.Unboxed" sampleUVVector (UV.length)
           , Length "Data.Vector.Storable" sampleSVVector (SV.length)
           , Length "Data.Sequence" sampleSeq (S.length)
           , Length "UnpackedFingerTree" sampleUnpackedFingerTree (getSum . UF.measureFingerTree)
           , Length "FingerTree" sampleFingerTree (getSum . F.measure)
           ])
    , bgroup
        "Stable Sort"
        (sorts
           [ Sort "Data.List" randomSampleList (L.sort)
           , Sort "Data.Vector" randomSampleVector sortVec
           , Sort "Data.Vector.Unboxed" randomSampleUVVector sortUVec
           , Sort "Data.Vector.Storable" randomSampleSVVector sortSVec
           , Sort "Data.Sequence" randomSampleSeq (S.sort)
           ])
    , bgroup
        "Replicate"
        (replicators
           [ Replicator "Data.List" L.replicate
           , Replicator "Data.Vector" V.replicate
           , Replicator "Data.Vector.Unboxed" UV.replicate
           , Replicator "Data.Vector.Storable" SV.replicate
           , Replicator "Data.Sequence" S.replicate
           ])
    , bgroup
        "Min"
        (mins
           [ Min "Data.List" (randomSampleList) (L.minimum)
           , Min "Data.Vector" (randomSampleVector) (V.minimum)
           , Min "Data.Vector.Unboxed" (randomSampleUVVector) (UV.minimum)
           , Min "Data.Vector.Storable" (randomSampleSVVector) (SV.minimum)
           ])
    , bgroup
        "Max"
        (maxs
           [ Max "Data.List" randomSampleList (L.maximum)
           , Max "Data.Vector" randomSampleVector (V.maximum)
           , Max "Data.Vector.Unboxed" randomSampleUVVector (UV.maximum)
           , Max "Data.Vector.Storable" randomSampleSVVector (SV.maximum)
           ])
    , bgroup
        "Filter Element"
        (let size = 10005
         in removeElems
              [ RemoveElement "Data.List" (sampleList size) (L.filter)
              , RemoveElement "Data.Vector" (sampleVector size) (V.filter)
              , RemoveElement
                  "Data.Vector.Unboxed"
                  (sampleUVVector size)
                  (UV.filter)
              , RemoveElement
                  "Data.Vector.Storable"
                  (sampleSVVector size)
                  (SV.filter)
              , RemoveElement "Data.Sequence" (sampleSeq size) (S.filter)
              ])
    , bgroup
        "Filter By Index"
        (let size = 10005
         in removeByIndexes
              [ RemoveByIndex "Data.Vector" (sampleVector size) (V.ifilter)
              , RemoveByIndex
                  "Data.Vector.Unboxed"
                  (sampleUVVector size)
                  (UV.ifilter)
              , RemoveByIndex
                  "Data.Vector.Storable"
                  (sampleSVVector size)
                  (SV.ifilter)
              ])
    ]
  where
    appends funcs =
      [ env
        (payload i)
        (\p -> bench (title ++ ":" ++ show i) $ whnf (\x -> forcer (func x x)) p)
      | i <- [10, 100, 1000, 10000]
      , Append title payload func forcer <- funcs
      ]
    conses funcs =
      [ env
        (sample i)
        (\p -> bench (title ++ ":" ++ show i) (whnf (\e -> func e p) 1))
      | i <- [10, 100, 1000, 10000]
      , Conser title sample func <- funcs
      ]
    replicators funcs =
      [ bench (title ++ ":" ++ show i) $ nf (\(x, y) -> func x y) (i, 1234)
      | i <- [10, 100, 1000, 10000]
      , Replicator title func <- funcs
      ]
    indexes funcs =
      [ env
        payload
        (\p -> bench (title ++ ":" ++ show index) $ nf (\x -> func p x) index)
      | index <- [10, 100, 1000, 10000]
      , Indexing title payload func <- funcs
      ]
    lengths funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Length title payload func <- funcs
      ]
    mins funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Min title payload func <- funcs
      ]
    maxs funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Max title payload func <- funcs
      ]
    sorts funcs =
      [ env
        (payload len)
        (\p -> bench (title ++ ":" ++ (show len)) $ nf (\x -> func x) p)
      | len <- [10, 100, 1000, 10000]
      , Sort title payload func <- funcs
      ]
    removeElems funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $ nf (\x -> func (/= relem) x) p)
      | relem <- [1, 100, 1000, 10000 :: Int]
      , RemoveElement title payload func <- funcs
      ]
    removeByIndexes funcs =
      [ env
        payload
        (\p ->
           bench (title ++ ":" ++ show relem) $
           nf (\x -> func (\index _ -> index /= relem) x) p)
      | relem <- [1, 100, 1000, 10000 :: Int]
      , RemoveByIndex title payload func <- funcs
      ]

sortVec :: V.Vector Int -> V.Vector Int
sortVec vec =
  runST
    (do mv <- V.thaw vec
        V.sort mv
        V.unsafeFreeze mv)

sortUVec :: UV.Vector Int -> UV.Vector Int
sortUVec vec =
  runST
    (do mv <- UV.thaw vec
        V.sort mv
        UV.unsafeFreeze mv)

sortSVec :: SV.Vector Int -> SV.Vector Int
sortSVec vec =
  runST
    (do mv <- SV.thaw vec
        V.sort mv
        SV.unsafeFreeze mv)

randomSampleList :: Int -> IO [Int]
randomSampleList i = evaluate $ force (take i (randoms (mkStdGen 0) :: [Int]))

randomSampleVector :: Int -> IO (V.Vector Int)
randomSampleVector i = evaluate $ force $ V.fromList (take i (randoms (mkStdGen 0) :: [Int]))

randomSampleUVVector :: Int -> IO (UV.Vector Int)
randomSampleUVVector i = evaluate $ force $ UV.fromList (take i (randoms (mkStdGen 0) :: [Int]))

randomSampleSVVector :: Int -> IO (SV.Vector Int)
randomSampleSVVector i = evaluate $ force $ SV.fromList (take i (randoms (mkStdGen 0) :: [Int]))

randomSampleSeq :: Int -> IO (S.Seq Int)
randomSampleSeq i = evaluate $ force $ S.fromList (take i (randoms (mkStdGen 0) :: [Int]))

sampleList :: Int -> IO [Int]
sampleList i = evaluate $ force [1..i]

sampleVector :: Int -> IO (V.Vector Int)
sampleVector i = evaluate $ force $ V.fromList [1..i]

sampleUVVector :: Int -> IO (UV.Vector Int)
sampleUVVector i = evaluate $ force $ UV.fromList [1..i]

sampleSVVector :: Int -> IO (SV.Vector Int)
sampleSVVector i = evaluate $ force $ SV.fromList [1..i]

sampleSeq :: Int -> IO (S.Seq Int)
sampleSeq i = evaluate $ force $ S.fromList [1..i]

sampleUnpackedFingerTree :: Int -> IO UF.FingerTree
sampleUnpackedFingerTree i = evaluate $ force $ UF.fromList [1..i]

sampleFingerTree :: Int -> IO (F.FingerTree (Sum Int) Int)
sampleFingerTree i = evaluate $ force $ F.fromList [1..i]

{-# INLINE ftIndex #-}
uftIndex :: UF.FingerTree -> Int -> Int
uftIndex ft i = case UF.lookupOrd (Sum i) ft of
  Nothing -> error "lol"
  Just x -> x

ftIndex :: F.FingerTree (Sum Int) Int -> Int -> Int
ftIndex ft i = case F.viewl (F.dropUntil (>= Sum i) ft) of
  F.EmptyL -> error "lol"
  x F.:< _ -> x

instance F.Measured (Sum Int) Int where
  measure _ = Sum 1
