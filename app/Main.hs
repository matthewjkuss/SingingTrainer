module Main where

  import Graphics.UI.GLUT
  import Data.IORef
  import Data.Maybe
  import Data.List
  import System.IO
  import System.Random
  import Control.Concurrent
  
  import Music
   
  {- TODO
  - 
  
  Possible extensions:
  - Press keyboard to play note
  - Play scale harmonicaly
  - Synthesia style song player
  -}
  
  type Point = (GLfloat, GLfloat, GLfloat) 
  
  main :: IO ()
  main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Singing Trainer"
    keyShift <- newIORef 0
    scaleNum <- newIORef 1
    tempo <- newIORef 60
    rangeL <- newIORef (-20)
    rangeR <- newIORef (20)
    keyboardMouseCallback $=
      Just (keyboardMouse rangeL rangeR tempo scaleNum keyShift)
    displayCallback $=
      (display rangeL rangeR tempo scaleNum keyShift)
    idleCallback $= Just idle
    mainLoop
  
  
  keyboardMouse :: IORef Int -> IORef Int -> IORef Int -> IORef Int
    -> IORef Int -> KeyboardMouseCallback
  keyboardMouse rangeL rangeR tempo scaleNum keyShift key Down _ _ = do
    scaleNum' <- get scaleNum
    scale <- return (snd $ scales !! (scaleNum'-1))
    case key of
      (SpecialKey KeyUp) -> do
        rangeL' <- get rangeL
        rangeR' <- get rangeR
        newKeyShift <- 
          if maximum scale - minimum scale > rangeR' - rangeL'
            then return Nothing
            else Just <$> randomRIO (rangeL' - minimum scale, rangeR' - maximum scale)
        keyShift $~! (\x-> case newKeyShift of Nothing -> x; Just y -> y)
        k <- get keyShift
        tempo' <- get tempo
        case newKeyShift of
          Just _-> mapM_ playNote (playScale (realToFrac tempo') scale k)
          Nothing -> return ()
        return ()
      (SpecialKey KeyLeft) ->
        keyShift $~! (\x -> if x + minimum scale > -39 then pred x else x)
      (SpecialKey KeyRight) -> do
        keyShift $~! (\x -> if x + maximum scale < 48 then succ x else x)
      (Char 'z') -> do
        scaleNum $~! (\x -> if x == 1 then length scales else pred x)
        scaleNum' <- get scaleNum
        scale <- return (snd $ scales !! (scaleNum'-1))
        keyShift $~!
          (\x -> if x + minimum scale < -39 then -39 - minimum scale else x)
      (Char 'x') -> do
        scaleNum $~! (\x -> if x == length scales then 1 else succ x)
        scaleNum' <- get scaleNum
        scale <- return (snd $ scales !! (scaleNum'-1))
        keyShift $~!
          (\x -> if x + maximum scale > 48 then 48 - maximum scale else x)
      (Char 'a') -> do
        tempo $~! (\x -> if x > 5 then x - 5 else x)
      (Char 's') -> do
        tempo $~! (\x -> if x < 300 then x + 5 else x)
      (Char 'q') -> do
        rangeL $~! (\x -> if x > -39 then pred x else x)
      (Char 'w') -> do
        rangeR' <- get rangeR
        rangeL $~! (\x -> if x < pred rangeR' then succ x else x)
      (Char 'e') -> do
        rangeL' <- get rangeL
        rangeR $~! (\x -> if x > succ rangeL' then pred x else x)
      (Char 'r') -> do
        rangeR $~! (\x -> if x < 48 then succ x else x)
      (SpecialKey KeyDown) -> do
        k <- get keyShift
        tempo' <- get tempo
        mapM_ playNote (playScale (realToFrac tempo') scale k)
        return ()
      _ -> return ()
    return ()
  
  keyboardMouse _ _ _ _ _ _ _ _ _ = return ()
  
  
  idle :: IdleCallback
  idle = do
    playMusic
    postRedisplay Nothing
  
  display :: IORef Int -> IORef Int -> IORef Int -> IORef Int
    -> IORef Int -> DisplayCallback
  display rangeL' rangeR' tempo' scaleNum' keyShift = do
    keyShift' <- get keyShift
    scaleNum <- get scaleNum'
    rangeL <- get rangeL'
    rangeR <- get rangeR'
    tempo <- get tempo'
    clear [ ColorBuffer ]
    color (Color3 255 255 (255::GLfloat))
    renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z)
        [(-1,-0.5,0),(-1,0,0),(1,0,0),(1,-0.5,0::GLfloat)]
  
    color (Color3 0 255 (0::GLfloat))
    
    drawWhiteScale keyShift' (snd $ scales !! (scaleNum-1))
  
    color (Color3 0 0 (0::GLfloat))
    renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (concat blackKeys)
  
    color (Color3 0 0 (0::GLfloat))
    renderPrimitive Lines $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  
    drawBlackScale keyShift' (snd $ scales !! (scaleNum-1))
    
    color (Color3 255 0 (0::GLfloat))
    rasterPos (Vertex2 (-0.1) (0.9::GLfloat))
    renderString Fixed9By15 ("SINGING TRAINER")
    rasterPos (Vertex2 (-0.1) (0.8::GLfloat))
    renderString Fixed9By15 ("BY MATTHEW KUSS")
    rasterPos (Vertex2 (0.5) (0.8::GLfloat))
    renderString Fixed9By15 ("Press LEFT or RIGHT to ")
    rasterPos (Vertex2 (0.5) (0.75::GLfloat))
    renderString Fixed9By15 ("change the pitch of the")
    rasterPos (Vertex2 (0.5) (0.7::GLfloat))
    renderString Fixed9By15 ("root note. Press DOWN to")
    rasterPos (Vertex2 (0.5) (0.65::GLfloat))
    renderString Fixed9By15 ("play the current scale,")
    rasterPos (Vertex2 (0.5) (0.6::GLfloat))
    renderString Fixed9By15 ("or UP to play a random")
    rasterPos (Vertex2 (0.5) (0.55::GLfloat))
    renderString Fixed9By15 ("note in the set range.")
    rasterPos (Vertex2 (-0.9) (0.4::GLfloat))
    renderString Fixed9By15 ("TEMPO (a & s): " ++ show tempo)
    rasterPos (Vertex2 (-0.9) (0.2::GLfloat))
    renderString Fixed9By15 ("RANGE LEFT  (q & w): " ++ noteName rangeL)
    rasterPos (Vertex2 (-0.9) (0.1::GLfloat))
    renderString Fixed9By15 ("RANGE RIGHT (e & r): " ++ noteName rangeR)
    rasterPos (Vertex2 (-0.9) (0.8::GLfloat))
    renderString Fixed9By15 ("SCALES (z & x):")
    mapM_ (\(n,(x,y)) -> do
      rasterPos (Vertex2 (-0.9::GLfloat) (0.8 - 0.3*n/(realToFrac $ length scales)))
      renderString Fixed9By15
        ((if realToFrac scaleNum == n then "[*] " else "[ ] ")
          ++ x
          ++ if realToFrac scaleNum == n
                then " (" ++ intercalate " " (noteName <$> (+keyShift') <$> y) ++ ")"
                else ""
          ))
        (zip [1..] scales)
  
    color (Color3 255 0 (255::GLfloat))
    renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (markKey rangeL)
    renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (markKey rangeR)
        
    swapBuffers
  
  markKey x = (twoToFour $ shrink $ fourToTwo (keyAt x !! 0) (keyAt x !! 2))
  
  shrink (x1,y1,x2,y2) = let w = (x2-x1) in (x1+w/4,y2+w/4+w,x2-w/4,y2+w/4)
  
  twoToFour (x1,y1,x2,y2) = [(x1,y1,0),(x1,y2,0),(x2,y2,0),(x2,y1,0)]
  
  fourToTwo (x1,y1,0) (x2,y2,0) = (x1,y1,x2,y2)
  
  drawWhiteKey :: Int -> IO ()
  drawWhiteKey keyShift' = 
    if not (isBlack keyShift')
    then renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (keyAt keyShift')
    else return ()
  
  drawBlackKey :: Int -> IO ()
  drawBlackKey keyShift' =
    if isBlack keyShift'
    then renderPrimitive Quads $
      mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (keyAt keyShift')
    else return ()
  
  drawWhiteScale :: Int -> [Int] -> IO ()
  drawWhiteScale keyShift' scale = do
    color (Color3 0 255 (0::GLfloat))
    drawWhiteKey (keyShift' + head scale)
    color (Color3 0 0 (255::GLfloat))
    mapM_ (\x -> drawWhiteKey $ keyShift' + x) (tail scale)
  
  drawBlackScale :: Int -> [Int] -> IO ()
  drawBlackScale keyShift' scale = do
    color (Color3 0 255 (0::GLfloat))
    drawBlackKey (keyShift' + head scale)
    color (Color3 0 0 (255::GLfloat))
    mapM_ (\x -> drawBlackKey $ keyShift' + x) (tail scale)
  
  
  isBlack :: Int -> Bool
  isBlack x =
    r == 1 || r == 3 || r == 6 || r == 8 || r == 10
    where r = mod (x + 0) 12
  
  myPoints :: [Point]
  myPoints =
    concat $ (\x->[(x,0,0),(x,-0.5,0)]) <$> (pred) <$> (*2) <$> (/52) <$> [0..52]
  
  keyAt :: Int -> [Point]
  keyAt x = (allKeys !! (x+40-1))
  
  allKeys :: [[Point]]
  allKeys = mergeSort $ middleC ++ blackKeys
  
  middleC :: [[Point]]
  middleC =
    (\x->[(x,0,0),(x,-0.5,0),(x+2/52,-0.5,0),(x+2/52,0,0)]) <$> pred <$> (*2)
      <$> (/52) <$> [0..52]
  
  takeBlacks :: Int -> Bool
  takeBlacks x =
    mod x 7 == 1 || mod x 7 == 2 || mod x 7 == 4 || mod x 7 == 5 || mod x 7 == 6
  
  noteName :: Int -> String
  noteName x =
    ["C","C","D","D","E","F","F","G","G","A","A","B"] !! (mod x 12)
    ++ show (div (x+48) 12)
    ++ if mod x 12 == 1 || mod x 12 == 3 || mod x 12 == 6 || mod x 12 == 8
          || mod x 12 == 10 then "#" else ""
  
  blackKeys :: [[Point]]
  blackKeys = 
    (\x->[(x-w,0,0),(x-w,-0.3,0),(x+w,-0.3,0),(x+w,0,0)])
      <$> (pred) <$> (*2) <$> (/52) <$> realToFrac
      <$> (take 36 $ drop 4 $ map (+(-5)) $ filter takeBlacks [1..9*7])
    where w = 1/(52*1.5)
  
  
  scales :: [(String, [Int])]
  scales = 
    [("Single Note", [0])
    ,("Major Octave Ascending", [0,4,7,12])
    ,("Major Octave Descending", [12,7,4,0])
    ,("Major Scale", [0,2,4,5,7,9,11,12])
    ,("Natural Minor Scale", [0,2,3,5,7,8,10,12])
    ,("Harmonic Minor Scale", [0,2,3,5,7,8,11,12])
    ,("Chromatic Scale", [0..12])
    ]
  
  
  