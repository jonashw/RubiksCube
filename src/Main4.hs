module Main4 where
import Data.Map (Map)
import qualified Data.Map as Map

data Cube = Cube { pieces :: [Piece] }
{-
 - A piece is composed of a position p at its center, and an arbitrary number of tiles
 -    ________
 -   |\       \
 -   | \_______\
 -   | |   p   |
 -   \ |       |
 -    \|_______|
 -}
data Piece = Piece { pos :: Vector, tiles :: [Tile] } deriving(Show,Eq)

{-
 - A tile is placed in 3D space by a position p at its center
 -   _______
 -  |       |
 -  |   p   |
 -  |_______|
 -}
data Tile = Tile { position :: Vector, rotation :: Vector, color :: Color } | NoTile deriving(Show,Eq)

{-
 - Coordinate system:
 -   X: left (0) -> right (2)
 -   Y: bottom (0) -> top (2)
 -   Z: back (0) -> front (2)
 -
 -             +y
 -              |  -z
 -              | /
 -      -y _____|/_____ +x
 -             /|
 -          +z/ |
 -             -y
 -
 -
 -}

data Color = R | O | Y | G | B | W | X deriving(Show,Eq)
data Vector = Vector { x :: Double, y :: Double, z :: Double } deriving(Show,Eq)

tileLength = 1
halfTileLength = tileLength / 2

makeCube' :: [Piece]
makeCube' = zipWith makePiece' cubePiecePositions cubePieceTileColors

makeCube :: [Piece]
makeCube = [ -- Front layer
             makePiece (Vector 0 2 2) W X X R G X, makePiece (Vector 1 2 2) W X X X G X, makePiece (Vector 2 2 2) W O X X G X
            ,makePiece (Vector 0 1 2) W X X R X X, makePiece (Vector 1 1 2) W X X X X X, makePiece (Vector 2 1 2) W O X X X X
            ,makePiece (Vector 0 0 2) W X X R X B, makePiece (Vector 1 0 2) W X X X X B, makePiece (Vector 2 0 2) W O X X X B ]
        ++ [ -- Middle layer
             makePiece (Vector 0 2 1) X X X R G X, makePiece (Vector 1 2 1) X X X X G X, makePiece (Vector 2 2 1) X O X X G X
            ,makePiece (Vector 0 1 1) X X X R X X,                                       makePiece (Vector 2 1 1) X O X X X X
            ,makePiece (Vector 0 0 1) X X X R X B, makePiece (Vector 1 0 1) X X X X X B, makePiece (Vector 2 0 1) X O X X G X ]
        ++ [ -- Back layer
             makePiece (Vector 0 2 0) X X Y R G X, makePiece (Vector 1 2 0) X X Y X G X, makePiece (Vector 2 2 0) X O Y X G X
            ,makePiece (Vector 0 1 0) X X Y R X X, makePiece (Vector 1 1 0) X X Y X X X, makePiece (Vector 2 1 0) X O Y X X X
            ,makePiece (Vector 0 0 0) X X Y R X B, makePiece (Vector 1 0 0) X X Y X X B, makePiece (Vector 2 0 0) X O Y X X B ]

-- build the cube positions starting with the top-front-left: left to right, top to bottom, front to back
cubePiecePositions :: [Vector]
cubePiecePositions = [ Vector xc yc zc | zc <- [2,1,0], yc <- [2,1,0], xc <- [0,1,2] ]

-- build the cube tile colors starting with the top-front-left: left to right, top to bottom, front to back
cubePieceTileColors :: [[Color]]
cubePieceTileColors = [ -- Front layer
                        [W,X,X,R,G,X], [W,X,X,X,G,X], [W,O,X,X,G,X]
                       ,[W,X,X,R,X,X], [W,X,X,X,X,X], [W,O,X,X,X,X]
                       ,[W,X,X,R,X,B], [W,X,X,X,X,B], [W,O,X,X,X,B] ]
                   ++ [ -- Middle layer
                        [X,X,X,R,G,X], [X,X,X,X,G,X], [X,O,X,X,G,X]
                       ,[X,X,X,R,X,X],                [X,O,X,X,X,X]
                       ,[X,X,X,R,X,B], [X,X,X,X,X,B], [X,O,X,X,G,X] ]
                   ++ [ -- Back layer
                        [X,X,Y,R,G,X], [X,X,Y,X,G,X], [X,O,Y,X,G,X]
                       ,[X,X,Y,R,X,X], [X,X,Y,X,X,X], [X,O,Y,X,X,X]
                       ,[X,X,Y,R,X,B], [X,X,Y,X,X,B], [X,O,Y,X,X,B] ]

makePiece' :: Vector -> [Color] -> Piece
makePiece' p [f,r,b,l,u,d] = makePiece p f r b l u d

-- a convenient piece constructor -> take the color of each face and yield a piece
-- order of pieces: Front, Right, Back, Left, Up, Down
makePiece :: Vector -> Color -> Color -> Color -> Color -> Color -> Color -> Piece
makePiece pos f r b l u d = Piece {pos = pos, tiles = [ makeTile f xe ye zp
                                                       ,makeTile r xp ye ze
                                                       ,makeTile b xe ye zn
                                                       ,makeTile l xn ye ze
                                                       ,makeTile u xe yp ze
                                                       ,makeTile d xe yn ze ]}
                                                       where
                                                         xe = x pos
                                                         ye = y pos
                                                         ze = z pos
                                                         xp = xe + halfTileLength
                                                         xn = xe - halfTileLength
                                                         yp = ye + halfTileLength
                                                         yn = ye - halfTileLength
                                                         zp = ze + halfTileLength
                                                         zn = ze - halfTileLength

makeTile :: Color -> Double -> Double -> Double -> Tile
makeTile X _ _ _ = NoTile
makeTile c xc yc zc = Tile (Vector xc yc zc) (Vector 0 0 0) c

showlist vl = mapM_ (putStrLn . show) vl


-- Querying
data Face = FrontFace | RightFace | BackFace | LeftFace | UpFace | DownFace deriving(Show,Eq,Ord)

facePieceCoordinateMap :: Map Face (Vector -> Double, Double)
facePieceCoordinateMap = Map.fromList[ (FrontFace, (z, 2))
                                      ,(RightFace, (x, 2))
                                      ,(BackFace,  (z, 0))
                                      ,(LeftFace,  (x, 0))
                                      ,(UpFace,    (y, 2))
                                      ,(DownFace,  (y, 0)) ]

facePieceTileCoordinateMap :: Map Face (Vector -> Double, Double -> Double)
facePieceTileCoordinateMap = Map.fromList [ (FrontFace, (z, (halfTileLength +)))
                                           ,(RightFace, (x, (halfTileLength +)))
                                           ,(BackFace,  (z, (halfTileLength -)))
                                           ,(LeftFace,  (x, (halfTileLength -)))
                                           ,(UpFace,    (y, (halfTileLength +)))
                                           ,(DownFace,  (y, (halfTileLength -))) ]


--getFaceColors :: [Piece] -> Face -> [Color]
--getFaceColors ps f = lookup f facePieceTileCoordinateMap
{-
getFaceColors :: [Piece] -> Face -> [Color]
getFaceColors ps f = map color $ map snd $ filter (\pt -> (cfn $ position t)) $ map (\p -> (pos p, tiles p)) $ getFacePieces ps f
                     where
                       fptcm = facePieceTileCoordinateMap Map.! f
                       cfn = fst fptcm
                       vfn = snd fptcm
-}

getPieceFaceColor :: Face -> Piece -> [Tile]
getPieceFaceColor f p = filter (\t -> (cfn $ position t) == expected) (tiles p)
                        where
                          fptcm = facePieceTileCoordinateMap Map.! f
                          cfn = fst fptcm
                          vfn = snd fptcm
                          expected = vfn $ cfn $ pos p

getFacePieces' :: [Piece] -> Face -> [Piece]
getFacePieces' ps f = filter (\p -> (fn $ pos p) == value) ps
                      where
                        fpcm = facePieceCoordinateMap Map.! f
                        fn = fst fpcm
                        value = snd fpcm

getFacePieces :: [Piece] -> Face -> [Piece]
getFacePieces ps FrontFace = filter (\p -> (z (pos p)) == 2) ps
getFacePieces ps RightFace = filter (\p -> (x (pos p)) == 2) ps
getFacePieces ps BackFace  = filter (\p -> (z (pos p)) == 0) ps
getFacePieces ps LeftFace  = filter (\p -> (x (pos p)) == 0) ps
getFacePieces ps UpFace    = filter (\p -> (y (pos p)) == 2) ps
getFacePieces ps DownFace  = filter (\p -> (y (pos p)) == 0) ps

testAssocList = [('a',1),('b',2),('c',3)]
testMap = Map.fromList [ ('a',1), ('b',2), ('c',3) ]
