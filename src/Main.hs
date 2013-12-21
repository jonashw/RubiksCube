module Main where

--data types
data Color = Red | Green | Orange | Yellow | Blue | White deriving(Show,Enum,Eq,Bounded)
data Piece = CenterPiece {front :: Color}
           | EdgePiece {front :: Color, top :: Color}
           | CornerPiece {front :: Color, top :: Color, left :: Color}
           | Core
           deriving(Show)
data LayerCode = FrontLayer | MiddleLayer | BackLayer deriving(Show,Enum,Bounded)--for building
data Layer = Layer {pieces :: [Piece]} deriving(Show)
data FaceCode = F | B | L | R | U | D deriving(Show,Enum,Eq,Bounded)
data Face = Face {center :: Color, _top :: Color, bottom :: Color, _left :: Color, right :: Color}
          deriving(Show)
-- is this possible? :: data Face = Face {color :: Color, neighbors :: Map Direction Face}
data SpindleNode = SpindleNode { __front :: Color, __back   :: Color
                                ,__top   :: Color, __bottom :: Color
                                ,__left  :: Color, __right  :: Color }
          deriving(Show)

type Triplet a = (a,a,a)

--convenience
centerColor :: FaceCode -> Color
centerColor F = White
centerColor B = Yellow
centerColor L = Red
centerColor R = Orange
centerColor U = Green
centerColor D = Blue

neighbors :: FaceCode -> [(FaceCode,FaceCode)]
neighbors F  = [(L,L),(R,R),(U,U),(D,D),(B,B),(F,F)]
neighbors B  = [(L,R),(R,L),(U,U),(D,D),(B,F),(F,F)]
neighbors L  = [(L,B),(R,F),(U,U),(D,D),(B,R),(F,F)]
neighbors R  = [(L,F),(R,B),(U,U),(D,D),(B,R),(F,F)]
neighbors U  = [(L,L),(R,R),(U,B),(D,F),(B,D),(F,F)]
neighbors D  = [(L,L),(R,R),(U,F),(D,B),(B,D),(F,F)]

neighbor :: FaceCode -> FaceCode -> (Maybe FaceCode)
neighbor faceCode neighborCode = lookup neighborCode $ neighbors faceCode

allColors = [minBound :: Color .. maxBound :: Color] -- cool :D
allFaceCodes = [minBound :: FaceCode .. maxBound :: FaceCode]

allFaces :: [Face]
allFaces = Prelude.map (\colors -> Face { center = colors !! 0
                                 ,_top   = colors !! 1
                                 ,bottom = colors !! 2
                                 ,_left  = colors !! 3
                                 ,right  = colors !! 4 }) [ [White, Green, Blue, Red, Orange]
                                                           ,[Orange, Green, Blue, White, Yellow]
                                                           ,[Yellow, Green, Blue, Orange, Red]
                                                           ,[Red, Green, Blue, Yellow, White]
                                                           ,[Green, Yellow, White, Red, Orange]
                                                           ,[Blue, Yellow, White, Orange, Red] ]

-- cube construction
{-
buildOuterLayer :: Face -> Bool -> Layer
buildOuterLayer f back = Layer f = Layer [ CornerPiece c t l ,EdgePiece   c t ,CornerPiece c r t
                                          ,EdgePiece   c l   ,CenterPiece c   ,EdgePiece   c r
                                          ,CornerPiece c b l ,EdgePiece   c b ,CornerPiece c r b ]
                                          where
                                            c = if back then () else (center f)
                                            t = if back then () else (_top f)
                                            b = if back then () else (bottom f)
                                            l = if back then () else (_left f)
                                            r = if back then () else (right f)

buildMiddleLayer :: Face -> Layer
buildMiddleLayer f = Layer [ CornerPiece c t l ,EdgePiece   c t ,CornerPiece c r t
                            ,EdgePiece   c l   ,Core            ,EdgePiece   c r
                            ,CornerPiece c b l ,EdgePiece   c b ,CornerPiece c r b ]
                            where
                              c = center f
                              t = _top f
                              b = bottom f
                              l = _left f
                              r = right f
-}

--standardCube = allColors

--display helpers
tabPrint :: Show a => a -> IO ()
tabPrint = putStrLn . ("\t" ++ ) . show

--let's go!
main = do
  putStrLn "Hey folks, this is my Rubiks Cube program.  Hope you like it lol"
  putStrLn ""
  putStrLn "Here are all the colors:"
  mapM_ tabPrint allColors
  putStrLn ""
  putStrLn "Here are all the faces:"
  mapM_ tabPrint allFaceCodes
