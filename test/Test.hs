{-# LANGUAGE LambdaCase, RecordWildCards, ScopedTypeVariables, ViewPatterns #-}
module Test where

import Rubik.Cube
import Rubik.Cube.Facelet.Internal
import Rubik.Cube.Cubie.Internal
import Rubik.Cube.Moves.Internal
import Rubik.Tables.Moves
import Rubik.Misc
import Rubik.Symmetry

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split (chunksOf)
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Primitive.Pinned as P
import Distribution.TestSuite
import Distribution.TestSuite.QuickCheck
import Test.HUnitPlus
import Test.QuickCheck
import qualified Test.QuickCheck as Gen
import System.Environment

-- If the test suite receives some command line arguments, only tests whose
-- fully qualified name has a prefix among them are run.
tests :: IO [Test]
tests = (filterTests . rename)
  [ testGroup "Cube"
    [ testGroup "Facelets"
      [ testProperty "permutation-to-facelet" $
          forAll (shuffle [0 .. 53]) (isJust . facelets')
      , testGroupInstance genFacelets
      , testProperty "facelet-colors" $
          forAll genCenteredFacelets (\(colorFaceletsOf -> c) ->
            (colorFacelets' . fromColorFacelets') c === Just c)
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
        [ testGenerator genUDEdgePermu2 (uDEdgePermu2 . fromUDEdgePermu2)
        , testCubeAction genUDEdgePermu2 genCubeUDFixFull
        ]
      , testGroup "EdgePermu2"
        [ testGenerator genEdgePermu2 (edgePermu . fromEdgePermu)
        ]
      , testGroup "FlipUDSlicePermu"
        [ testConjugate genCubeUDFixSym genCubeFull conjugateFlipUDSlicePermu
        ]
      , testGroup "ToFacelet"
        [ testGroupMorphism genCubeFull toFacelet
        ]
      ]
    , testGroup "Coord"
      [ testCoord "CornerPermu"
          genCornerPermu (cornerPermu . fromCornerPermu)
      , testCoord "CornerOrien"
          genCornerOrien (cornerOrien . fromCornerOrien)
      , testCoord "EdgePermu"
          genEdgePermu (edgePermu . fromEdgePermu)
      , testCoord "EdgeOrien"
          genEdgeOrien (edgeOrien . fromEdgeOrien)
      , testCoord "UDSlicePermu"
          genUDSlicePermu (uDSlicePermu . fromUDSlicePermu)
      , testCoord "UDSlice"
          genUDSlice (uDSlice . fromUDSlice)
      , testCoord "UDSlicePermu2"
          genUDSlicePermu2 (uDSlicePermu2 . fromUDSlicePermu2)
      , testCoord "UDEdgePermu2"
          genUDEdgePermu2 (uDEdgePermu2 . fromUDEdgePermu2)
      , testCoord "FlipUDSlicePermu"
          genFlipUDSlicePermu Just
      ]
    , testGroup "Moves"
      [ testMoves ""
          "UUUUUUUUU LLLLLLLLL FFFFFFFFF RRRRRRRRR BBBBBBBBB DDDDDDDDD"
      , testMoves "uuuu"
          "UUUUUUUUU LLLLLLLLL FFFFFFFFF RRRRRRRRR BBBBBBBBB DDDDDDDDD"
      , testMoves "u"
          "UUUUUUUUU FFFLLLLLL RRRFFFFFF BBBRRRRRR LLLBBBBBB DDDDDDDDD"
      , testMoves "l"
          "BUUBUUBUU LLLLLLLLL UFFUFFUFF RRRRRRRRR BBDBBDBBD FDDFDDFDD"
      , testMoves "f"
          "UUUUUULLL LLDLLDLLD FFFFFFFFF URRURRURR BBBBBBBBB RRRDDDDDD"
      , testMoves "r"
          "UUFUUFUUF LLLLLLLLL FFDFFDFFD RRRRRRRRR UBBUBBUBB DDBDDBDDB"
      , testMoves "b"
          "RRRUUUUUU ULLULLULL FFFFFFFFF RRDRRDRRD BBBBBBBBB DDDDDDLLL"
      , testMoves "d"
          "UUUUUUUUU LLLLLLBBB FFFFFFLLL RRRRRRFFF BBBBBBRRR DDDDDDDDD"
      , testMoves "ulfrbd"
          "LBBBURFFR ULRULDDDD UUBFFDBLD UULRRDFFD UUFBBLRRF LFRLDRLBB"
      , testCube "sURF" surf3
          "FFFFUFFFF DDDDLDDDD RRRRFRRRR UUUURUUUU LLLLBLLLL BBBBDBBBB"
      , testCube "sF" sf2
          "DDDDUDDDD RRRRLRRRR FFFFFFFFF LLLLRLLLL BBBBBBBBB UUUUDUUUU"
      , testCube "sU" su4
          "UUUUUUUUU FFFFLFFFF RRRRFRRRR BBBBRBBBB LLLLBLLLL DDDDDDDDD"
      , testCube "sLR" slr2
          "UUUUUUUUU RRRRLRRRR FFFFFFFFF LLLLRLLLL BBBBBBBBB DDDDDDDDD"
      ]
    ]
  , testGroup "Tables"
    [ testGroup "Moves"
      [ testMoveTables "move18CornerPermu"
          move18 move18CornerPermu
      , testMoveTables "move18CornerOrien"
          move18 move18CornerOrien
      , testMoveTables "move18EdgeOrien"
          move18 move18EdgeOrien
      , testMoveTables "move18UDSlicePermu"
          move18 move18UDSlicePermu
      , testMoveTables "move18UDSlice"
          move18 move18UDSlice
      , testMoveTables "move10UDSlicePermu2"
          move10 move10UDSlicePermu2
      , testMoveTables "move10UDEdgePermu2"
          move10 move10UDEdgePermu2
      ]
    , testUDSlicePermu
    , testFlipUDSlicePermu
    , testRawToSymFlipUDSlicePermu
    , testSymReprTable "srFUDSP"
        reprFlipUDSlicePermu conjugateFlipUDSlicePermu
    , testMoveSymTables "msFUDSP" move18 move18SymFlipUDSlicePermu
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
genCornerOrien = unsafeCornerOrien'
  . (\x -> (3 - sum x) `mod` 3 : x) <$> replicateM 7 (Gen.choose (0, 2))
genCornerOrienFull = unsafeCornerOrien' <$> replicateM 8 (Gen.choose (0,5))
genCorner = liftA2 Corner genCornerPermu genCornerOrien
genCornerFull = liftA2 Corner genCornerPermu genCornerOrienFull
genEdgePermu = unsafeEdgePermu' <$> shuffle [0 .. 11]
genEdgeOrien = unsafeEdgeOrien'
  . (\x -> sum x `mod` 2 : x) <$> replicateM 11 (Gen.choose (0, 1))
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
genFlipUDSlicePermu = liftA2 (,) genUDSlicePermu genEdgeOrien
genCubeUDFixFull = liftA2 Cube genCornerFull genEdge2
genCubeUDFixSym = elements sym16'

testConjugate :: (FromCube a, Eq a, Show a)
  => Gen Cube -> Gen Cube -> (Cube -> a -> a) -> Test
testConjugate genSym genCube conj
  = testProperty "conjugate" $
      forAll genSym $ \s -> forAll genCube $ \c ->
        fromCube (inverse s <> c <> s) === conj s (fromCube c)

-- * Coord

testCoord :: forall a. (RawEncodable a, Show a, Eq a)
  => String -> Gen a -> (a -> Maybe a) -> Test
testCoord name gen check = testGroup name $
  [ testProperty "coord-bijection-1" $
      forAll genCoord $ join ((===) . encode . decode)
  , testProperty "coord-bijection-2" $
      forAll gen $ join ((===) . decode . encode)
  , testProperty "coord-range" $
      forAll gen $ liftA2 (&&) (range gen >) (>= 0) . unRawCoord . encode
  , testProperty "coord-correct" $
      forAll genCoord $ isJust . check . decode
  ]
  where
    genCoord = RawCoord <$> Gen.choose (0, range gen-1) :: Gen (RawCoord a)

testMoveTables :: (CubeAction a, RawEncodable a)
  => String -> MoveTag m [Cube] -> MoveTag m [RawMove a]
  -> Test
testMoveTables name (MoveTag cubes) (MoveTag moves)
  = testProperty name $
      conjoin $ zipWith propMoveTable1 cubes moves

propMoveTable1 :: forall a. (CubeAction a, RawEncodable a)
  => Cube -> RawMove a -> Property
propMoveTable1 c m'@(RawMove m)
  = forAll genCoord $ \x ->
      RawCoord (m P.! unRawCoord x)
      === (encode . (`cubeAction` c) . decode) x
  where
    genCoord = RawCoord <$> Gen.choose (0, range m'-1) :: Gen (RawCoord a)

-- * Moves

testMoves :: String -> String -> Test
testMoves moves result = '.' : moves ~:
  (stringOfCubeColors . moveToCube <$> stringToMove moves) ~?= Right result

testCube :: String -> Cube -> String -> Test
testCube name c result = name ~: stringOfCubeColors c ~?= result

-- * Move tables

-- ** FlipUDSlice implementation

testUDSlicePermu
  = testProperty "UDSlicePermu" $
      forAll (Gen.choose (0, 15)) $ \c -> forAll genUDSlicePermu $ \udsp ->
        conjugateUDSlicePermu (sym16' !! c) udsp
        === conjugateUDSlicePermu' (SymCode c) udsp

testFlipUDSlicePermu
  = testProperty "FlipUDSlicePermu" $
      forAll (Gen.choose (0, 15)) $ \c -> forAll genFlipUDSlicePermu $ \fudsp ->
        counterexample ((show $ sym16' !! c) ++ "XXQS") $
        conjugateFlipUDSlicePermu (sym16' !! c) fudsp
        === conjugateFlipUDSlicePermu' (SymCode c) fudsp

testRawToSymFlipUDSlicePermu
  = testProperty "raw-to-sym-fudsp" $
      forAll genCoordFUDSP $ \z ->
        let (SymClass c, sc) = rawToSymFlipUDSlicePermu z
        in encode
            ( conjugateFlipUDSlicePermu' sc
            . decode . RawCoord
            $ unSymClassTable classFlipUDSlicePermu P.! c)
          === z
  where
    genCoordFUDSP = RawCoord <$> Gen.choose (0, range ([] :: [FlipUDSlicePermu]) -1)

testMoveSymTables :: ()
  => String -> MoveTag m [Cube] -> MoveTag m [SymMove UDFix FlipUDSlicePermu]
  -> Test
testMoveSymTables name (MoveTag cubes) (MoveTag moves)
  = testProperty name $
      conjoin $ zipWith propMoveSymTable1 cubes moves

propMoveSymTable1 c (SymMove m)
  -- = forAll (Gen.choose (0, P.length m-1)) $ \x ->
  = case G.find (\x -> x >= 16 * P.length m) m of
      Nothing -> property True
      Just x -> counterexample (show (x, P.length m)) False

testSymReprTable name (SymReprTable repr) conj
  = testProperty name $
      forAll (Gen.choose (0, P.length repr-1)) $ \x ->
        let y = repr P.! x
            (r, i) = y `divMod` 16
        in (encode . conj (sym16' !! i) . decode . RawCoord) r
          === RawCoord x

-- * Typeclass laws

testMonoid0 :: (Monoid a, Eq a, Show a) => proxy a -> Test
testMonoid0 proxy =
  "mempty-mappend-mempty" ~:
    mempty <> mempty ~?= mempty `asProxyTypeOf` proxy

testMonoid :: (Monoid a, Eq a, Show a) => Gen a -> Test
testMonoid gen = testGroup "Monoid"
  [ testProperty "left-identity" $
      forAll gen (\x -> mempty <> x === x)
  , testProperty "right-identity" $
      forAll gen (\x -> x <> mempty === x)
  , testProperty "associativity" $
      forAll gen $ \x -> forAll gen $ \y -> forAll gen $ \z ->
        (x <> y) <> z === x <> (y <> z)
  , testMonoid0 gen
  ]

testGroup0 :: (Group a, Eq a, Show a) => proxy a -> Test
testGroup0 proxy =
  "inverse-mempty" ~:
    inverse mempty ~?= mempty `asProxyTypeOf` proxy

testGroupInstance :: (Group a, Eq a, Show a) => Gen a -> Test
testGroupInstance gen = testGroup "Group"
  [ testProperty "inverse-left" $
      forAll gen (\x -> inverse x <> x === mempty)
  , testProperty "inverse-right" $
      forAll gen (\x -> x <> inverse x === mempty)
  , testGroup0 gen
  , testMonoid gen
  ]

testMonoidMorphism :: (Monoid a, Monoid b, Eq a, Eq b, Show a, Show b)
  => Gen a -> (a -> b) -> Test
testMonoidMorphism gen f = testGroup "MonoidM"
  [ "morphism-iden" ~: f mempty ~?= mempty
  , testProperty "morphism-compose" $
      forAll gen $ \x -> forAll gen $ \y ->
        f (x <> y) === f x <> f y
  ]

testGroupMorphism :: (Group a, Group b, Eq a, Eq b, Show a, Show b)
  => Gen a -> (a -> b) -> Test
testGroupMorphism gen f = testGroup "GroupM"
  [ testMonoidMorphism gen f
  , testProperty "morphism-inverse" $
      forAll gen $ \x -> (inverse . f) x === (f . inverse) x
  ]

testCubeAction
  :: (CubeAction a, FromCube a, Eq a, Show a)
  => Gen a -> Gen Cube -> Test
testCubeAction gen genCube = testGroup "CubeAction"
  [ testProperty "id-cube-action" $
      forAll gen $ \x -> cubeAction x iden === x
  , testProperty "from-cube-action" $
      forAll genCube $ \x -> forAll genCube $ \c ->
        cubeAction (fromCube x) c === fromCube (x <> c) `asProxyTypeOf` gen
  ]

testGenerator :: (Eq a, Show a) => Gen a -> (a -> Maybe b) -> Test
testGenerator gen p = testProperty "generator" $ forAll gen (isJust . p)

-- * Utilities

-- Qualify test names
rename :: [Test] -> [Test]
rename = fmap (rename' "")

rename' :: String -> Test -> Test
rename' pfx (Test t) = Test t{ name = pfx ++ name t }
rename' pfx (Group name conc tests)
  = Group name conc (fmap (rename' (pfx ++ name ++ "/")) tests)
rename' pfx (ExtraOptions opts test) = ExtraOptions opts (rename' pfx test)

filterTests :: [Test] -> IO [Test]
filterTests tests = do
  getArgs <&> \case
    [] -> tests
    pfxs -> filterTests' pfxs tests

filterTests' pfxs = (>>= filterTest pfxs)

filterTest pfxs test@(Test t) = [test | any (`isPrefixOf` name t) pfxs]
filterTest pfxs (Group name conc tests)
  = let tests' = filterTests' pfxs tests
    in [Group name conc tests' | (not . null) tests']
filterTest pfxs (ExtraOptions opts test) = ExtraOptions opts <$> filterTest pfxs test
