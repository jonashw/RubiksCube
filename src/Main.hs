module Main where

--data types
data Color = Red | Green | Orange | Yellow | Blue | White deriving(Show,Enum,Eq)
data FaceCode = F | B | L | R | U | D deriving(Show,Enum,Eq,Bounded)
data Piece = CenterPiece {front :: Color}
           | EdgePiece {front :: Color, top :: Color}
           | CornerPiece {front :: Color, top :: Color, left :: Color}
		   | Core
		   deriving(Show)
data Spindle = Spindle {pieces :: [Piece]} deriving(Show)
data Face = Face {center :: Color, _top :: Color, bottom :: Color, _left :: Color, right :: Color}
            deriving(Show)

type Triplet a = (a,a,a)

--convenience 
allColors :: [Color]
allColors = [White,Orange,Yellow,Red,Green,Blue]

allFaceCodes :: [FaceCode]
allFaceCodes = [F,B,L,R,U,D]

allFaces :: [Face]
allFaces = map faceFromColorList [ [White, Green, Blue, Red, Orange]
                                  ,[Orange, Green, Blue, White, Yellow]
                                  ,[Yellow, Green, Blue, Orange, Red]
                                  ,[Red, Green, Blue, Yellow, White]
                                  ,[Green, Yellow, White, Red, Orange]
                                  ,[Blue, Yellow, White, Orange, Red] ]

faceFromColorList :: [Color] -> Face
faceFromColorList colors = Face { center = colors !! 0
                                 ,_top   = colors !! 1
                                 ,bottom = colors !! 2
                                 ,_left  = colors !! 3
                                 ,right  = colors !! 4 }

-- cube construction
--build a spindle left-to-right, top-to-bottom
buildSpindle :: Face -> Spindle
buildSpindle f = Spindle [ CornerPiece c t l ,EdgePiece   c t ,CornerPiece c r t
                          ,EdgePiece   c l   ,CenterPiece c   ,EdgePiece   c r
                          ,CornerPiece c b l ,EdgePiece   c b ,CornerPiece c r b ]
                          where
                            c = center f
                            t = _top f
                            b = bottom f
                            l = _left f
                            r = right f

--this is supposed to build a layer of the cube, taking a particular starting face and a layer index (0-2)
buildLayer :: Face -> Int -> Triplet(Triplet(Piece))
buildLayer f i = ( (Core,Core,Core)
                  ,(Core,Core,Core)
                  ,(Core,Core,Core) )

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
