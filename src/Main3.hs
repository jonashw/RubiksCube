module Main3 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Fixed
import Test.HUnit
import Test.QuickCheck

data Piece = Piece {tiles :: Map Face Tile, position :: Vector, rotation :: Vector} deriving (Show)
data Tile = Red | Orange | Yellow | Green | Blue | White | BlankTile deriving(Show,Eq)
data Face = Face Axis Extreme deriving(Show,Ord,Eq)
data Axis = X | Y | Z deriving(Show,Ord,Eq)
data Extreme = Min | Max deriving(Show,Ord,Eq)
data Vector = Vector { x :: Double, y :: Double, z :: Double } deriving(Show)

--
-- constructors
--
--
cubePieces :: [Piece]
cubePieces = concat $ map zLayerPieces [0,1,2]

-- layer constructors
zLayerPieces :: Double -> [Piece]
zLayerPieces z = zipWith3 Piece (zLayerTileMaps z) (zLayerPositions z) (zLayerRotations z)

zLayerTileMaps :: Double -> [Map Face Tile]
zLayerTileMaps 0 = backLayerTileMaps
zLayerTileMaps 1 = middleLayerTileMaps
zLayerTileMaps 2 = frontLayerTileMaps
zLayerTileMaps _ = error("valid z-values are 0-2")

xLayerPositions x = [ Vector x y z | y <- [0,1,2], z <- [0,1,2] ]
yLayerPositions y = [ Vector x y z | z <- [0,1,2], x <- [0,1,2] ]
zLayerPositions z = [ Vector x y z | y <- [0,1,2], x <- [0,1,2] ]

zLayerRotations :: Double -> [Vector]
zLayerRotations 0 = backLayerRotations
zLayerRotations 1 = middleLayerRotations
zLayerRotations 2 = frontLayerRotations
zLayerRotations _ = error("valid z-values are 0-2")

frontLayerRotations =  [Vector 0 0 0,   Vector 0 0 0,   Vector 0 0 90,
                        Vector 0 0 270, Vector 0 0 0,   Vector 0 0 90,
                        Vector 0 0 270, Vector 0 0 180, Vector 0 0 180]

frontLayerTileMaps =   [cornerTiles White Green Red, edgeTiles White Green, cornerTiles White Green Orange,
                        edgeTiles White Red,         centerTiles White,     edgeTiles White Orange,
                        cornerTiles White Red Blue,  edgeTiles White Blue,  cornerTiles White Blue Orange]

middleLayerRotations = [Vector 0 270 0,   Vector 90 0 0,  Vector 0 90 0,
                        Vector 0 0 270,   Vector 0 0 0,   Vector 0 0 90,
                        Vector 180 270 0, Vector 270 0 0, Vector 180 90 0]

middleLayerTileMaps =  [edgeTiles Red Green, centerTiles Green,     edgeTiles Orange Green,
                        centerTiles Red,     centerTiles BlankTile, centerTiles Orange,
                        edgeTiles White Red, centerTiles Blue,      edgeTiles Blue Orange]

backLayerRotations =  [Vector 0 180 0,   Vector 0 180 0,   Vector 0 180 90,
                       Vector 0 180 270, Vector 0 180 0,   Vector 0 180 90,
                       Vector 0 180 270, Vector 0 180 180, Vector 0 180 180]

backLayerTileMaps =    [cornerTiles Yellow Green Orange, edgeTiles Yellow Green, cornerTiles Red Green Yellow,
                        edgeTiles Yellow Red,            centerTiles Yellow,     edgeTiles Yellow Red,
                        cornerTiles Yellow Orange Blue,  edgeTiles Yellow Blue,  cornerTiles Yellow Blue Red]

-- tile map constructors for each piece type
cornerTiles :: Tile -> Tile -> Tile -> Map Face Tile
cornerTiles f u l = Map.fromList [ (Face Z Max, f), (Face Y Max, u), (Face X Min, l) ]
--
edgeTiles :: Tile -> Tile -> Map Face Tile
edgeTiles f u = Map.fromList [ (Face Z Max, f), (Face Y Max, u) ]
--
centerTiles :: Tile -> Map Face Tile
centerTiles f = Map.fromList [ (Face Z Max, f) ]


--
-- queries
--
--zLayerTiles :: [Piece] -> Double -> [Tile]
--zLayerTiles ps z =
pieceTile :: Piece -> Face -> Maybe Tile
pieceTile p pf | pieceIsQueryable p = Map.lookup pf (tiles p)
               |          otherwise = Nothing

-- maps an absolute face to the face of a rotated piece
--pieceRotatedFace :: Piece -> Face -> Face
--pieceRotatedFace p f =

pieceIsQueryable :: Piece -> Bool
pieceIsQueryable p = rotationIsRight $ rotation p

rotationIsRight :: Vector -> Bool
rotationIsRight r = all rightAngle $ map (\f -> f r) [x,y,z]

rightAngle :: Double -> Bool
rightAngle angle = (mod' angle 90) == 0


--
--convenience
--
showlist vl = mapM_ (putStrLn . show) vl

--
-- demos
--
zero :: Vector
zero = Vector 0 0 0

notZero :: Vector
notZero = Vector 12 3.4 3

zeroPiece :: Piece
zeroPiece = Piece cornerPieceTileMap zero zero

notZeroPiece = Piece cornerPieceTileMap notZero notZero

cornerPieceTileMap :: Map Face Tile
cornerPieceTileMap = Map.fromList [ (Face X Min,Red), (Face Y Max,Blue), (Face Z Max,White) ]


--
-- Tests
--
tests = TestList $ map TestCase
  [assertEqual "add tests here" 1  1
  ]

prop_empty c1 = (c1 :: Int) == c1

runTests = do
  -- runTestTT tests
  -- quickCheck prop_empty
  return ()

main :: IO ()
main = runTests
