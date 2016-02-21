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
        [ testGenerator genCornerPermu (cornerPermu . fromCornerPermu)
        , testGroupInstance genCornerPermu
        , testCubeAction genCornerPermu genCubeFull
        ]
      , testGroup "CornerOrien"
        [ testGenerator genCornerOrienFull (cornerOrien . fromCornerOrien)
        , testCubeAction genCornerOrienFull genCubeFull
        ]
      , testGroup "Corner"
        [ testGroupInstance genCornerFull
        , testCubeAction genCornerFull genCubeFull
        ]
      , testGroup "EdgePermu"
        [ testGenerator genEdgePermu (edgePermu . fromEdgePermu)
        , testGroupInstance genEdgePermu
        , testCubeAction genEdgePermu genCube
        ]
      , testGroup "EdgeOrien"
        [ testGenerator genEdgeOrien (edgeOrien . fromEdgeOrien)
        ]
      , testGroup "Edge"
        [ testGroupInstance genEdge
        , testCubeAction genEdge genCube
        ]
      , testGroup "Cube"
        [ testGroupInstance genCubeFull
        ]
      , testGroup "UDSlicePermu"
        [ testGenerator genUDSlicePermu (uDSlicePermu . fromUDSlicePermu)
        , testCubeAction genUDSlicePermu genCube
        ]
      , testGroup "UDSlice"
        [ testGenerator genUDSlice (uDSlice . fromUDSlice)
        , testCubeAction genUDSlice genCube
        ]
      , testGroup "UDSlicePermu2"
        [ testGenerator genUDSlicePermu2 (uDSlicePermu2 . fromUDSlicePermu2)
        , testCubeAction genUDSlicePermu2 genCubeUDFixFull
        ]
      , testGroup "UDEdgePermu2"
        [ testGenerator genUDSlicePermu2 (uDSlicePermu2 . fromUDSlicePermu2)
        , testCubeAction genUDSlicePermu2 genCubeUDFixFull
        ]
      , testGroup "EdgePermu2"
        [ testGenerator genEdgePermu2 (edgePermu . fromEdgePermu)
        ]
      , testGroup "ToFacelet"
        [ testGroupMorphism genCubeFull toFacelet
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
genCube = liftA2 Cube genCorner genEdge
genCubeFull = liftA2 Cube genCornerFull genEdge
genCubeSolvable = genCube `suchThat` solvable
genUDSlicePermu = unsafeUDSlicePermu' . take 4 <$> shuffle [0 .. 11]
genUDSlice = unpermuUDSlice <$> genUDSlicePermu
genUDSlicePermu2 = unsafeUDSlicePermu2' <$> shuffle [0 .. 3]
genUDEdgePermu2 = unsafeUDEdgePermu2' <$> shuffle [0 .. 7]
genEdgePermu2 = liftA2 edgePermu2 genUDSlicePermu2 genUDEdgePermu2
genEdge2 = liftA2 Edge genEdgePermu2 genEdgeOrien
genCubeUDFixFull = liftA2 Cube genCornerFull genEdge2

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

testMonoidMorphism :: (Monoid a, Monoid b, Eq a, Eq b, Show a, Show b)
  => Gen a -> (a -> b) -> Test
testMonoidMorphism gen f = testGroup "MonoidM"
  [ "morphism-iden" ~: f mempty ~?= mempty
  , testProperty "morphism-compose" $
      forAll gen $ \x -> forAll gen $ \y ->
        f (x <> y) == f x <> f y
  ]

testGroupMorphism :: (Group a, Group b, Eq a, Eq b, Show a, Show b)
  => Gen a -> (a -> b) -> Test
testGroupMorphism gen f = testGroup "GroupM"
  [ testMonoidMorphism gen f
  , testProperty "morphism-inverse" $
      forAll gen $ \x -> (inverse . f) x == (f . inverse) x
  ]

testCubeAction
  :: (CubeAction a, FromCube a, Eq a, Show a)
  => Gen a -> Gen Cube -> Test
testCubeAction gen genCube = testGroup "CubeAction"
  [ testProperty "id-cube-action" $
      forAll gen $ \x -> cubeAction x iden == x
  , testProperty "from-cube-action" $
      forAll genCube $ \x -> forAll genCube $ \c ->
        cubeAction (fromCube x) c == fromCube (x <> c) `asProxyTypeOf` gen
  ]

testGenerator :: (Eq a, Show a) => Gen a -> (a -> Maybe b) -> Test
testGenerator gen p = testProperty "generator" $ forAll gen (isJust . p)

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
