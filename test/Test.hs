{-# LANGUAGE ViewPatterns #-}
module Test where

import Rubik.Cube.Facelet.Internal
import Rubik.Cube.Cubie.Internal
import Rubik.Misc

import Control.Applicative
import Control.Monad
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Monoid
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import Test.HUnitPlus
import Test.QuickCheck
import qualified Test.QuickCheck as Gen

tests :: IO [Test]
tests = (return . rename)
  [ testGroup "Cube"
    [ testGroup "Facelets"
      [ testProperty "permutation-to-facelet" $
          forAll (shuffle [0 .. 53]) (isJust . facelets')
      , testGroupInstance genFacelets
      , testProperty "facelet-colors" $
          forAll genCenteredFacelets (\(colorFaceletsOf -> c) ->
            (colorFacelets' . fromColorFacelets') c == Just c)
      ]
    , testGroup "Cubie"
      [ testGroup "CornerPermu"
        [ testGroupInstance genCornerPermu
        ]
      , testGroup "Corner"
        [ testGroupInstance genCornerFull
        ]
      , testGroup "EdgePermu"
        [ testGroupInstance genEdgePermu
        ]
      , testGroup "Edge"
        [ testGroupInstance genEdge
        ]
      ]
    , testGroup "Coord" []
    , testGroup "*" []
    ]
  ]

-- * Facelets

genFacelets = unsafeFacelets' <$> shuffle [0 .. 53]

-- | Centers remain fixed
genCenteredFacelets = unsafeFacelets' <$> do
  let chunks = chunksOf 9 [4 .. 53]
  shuffled <- (shuffle . ([0 .. 3] ++) . concat . fmap tail) chunks
  let (x, y) = splitAt 4 shuffled
      facelets = (x ++) . concat . zipWith (:) (fmap head chunks) . chunksOf 8
  return (facelets y)

-- * Cubies

genCornerPermu = unsafeCornerPermu' <$> shuffle [0 .. 7]
genCornerOrien = unsafeCornerOrien' <$> replicateM 8 (Gen.choose (0, 2))
genCornerOrienFull = unsafeCornerOrien' <$> replicateM 8 (Gen.choose (0,5))
genCorner = liftA2 Corner genCornerPermu genCornerOrien
genCornerFull = liftA2 Corner genCornerPermu genCornerOrienFull
genEdgePermu = unsafeEdgePermu' <$> shuffle [0 .. 11]
genEdgeOrien = unsafeEdgeOrien' <$> replicateM 12 (Gen.choose (0, 1))
genEdge = liftA2 Edge genEdgePermu genEdgeOrien

-- * Typeclass laws

testMonoid0 :: (Monoid a, Eq a, Show a) => proxy a -> Test
testMonoid0 proxy =
  "mempty-mappend-mempty" ~:
    mempty <> mempty ~?= mempty `asProxyTypeOf` proxy

testMonoid :: (Monoid a, Eq a, Show a) => Gen a -> Test
testMonoid gen = testGroup "Monoid"
  [ testProperty "left-identity" $
      forAll gen (\x -> mempty <> x == x)
  , testProperty "right-identity" $
      forAll gen (\x -> x <> mempty == x)
  , testProperty "associativity" $
      forAll gen $ \x -> forAll gen $ \y -> forAll gen $ \z ->
        (x <> y) <> z == x <> (y <> z)
  , testMonoid0 gen
  ]

testGroup0 :: (Group a, Eq a, Show a) => proxy a -> Test
testGroup0 proxy =
  "inverse-mempty" ~:
    inverse mempty ~?= mempty `asProxyTypeOf` proxy

testGroupInstance :: (Group a, Eq a, Show a) => Gen a -> Test
testGroupInstance gen = testGroup "Group"
  [ testProperty "inverse-left" $
      forAll gen (\x -> inverse x <> x == mempty)
  , testProperty "inverse-right" $
      forAll gen (\x -> x <> inverse x == mempty)
  , testGroup0 gen
  , testMonoid gen
  ]

-- * Utilities

asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const

-- Qualify test names
rename :: [Test] -> [Test]
rename = fmap (rename' "")

rename' :: String -> Test -> Test
rename' pfx (Test t) = Test t{ name = pfx ++ name t }
rename' pfx (Group name conc tests)
  = Group name conc (fmap (rename' (pfx ++ name ++ "/")) tests)
rename' pfx (ExtraOptions opts test) = ExtraOptions opts (rename' pfx test)
