module Goursat
  ( main )
  where
import           Data.IORef
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           Utils.OpenGL

red :: Color4 GLfloat
red = Color4 1 0 0 1

fGoursat :: Double -> Double -> XYZ -> Double
fGoursat a b (x,y,z) =
  x**4 + y**4 + z**4 + a*(x**2+y**2+z**2)**2 + b*(x**2+y**2+z**2)

trianglesGoursat :: Double -> Double -> Double
                 -> IO [((Vertex3 GLfloat, Vertex3 GLfloat, Vertex3 GLfloat), Normal3 GLfloat)]
trianglesGoursat a b l = do
  triangles <- marchingCubes (fGoursat a b) l (-2.5) 2.5 50
  return $ map fromTriangle triangles

display :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
        -> IORef Double -> IORef Double -- parameters a and b
        -> IORef Double  -- isolevel
        -> IORef Double  -- zoom
        -> DisplayCallback
display rot1 rot2 rot3 a b l zoom = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get rot1
  r2 <- get rot2
  r3 <- get rot3
  z <- get zoom
  (_, size) <- get viewport
  a' <- get a
  b' <- get b
  l' <- get l
  triangles <- trianglesGoursat a' b' l'
  loadIdentity
  resize z size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $ mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3), norm) = do
      materialDiffuse FrontAndBack $= red
      normal norm
      vertex v1
      vertex v2
      vertex v3

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (w'/h') 1.0 100.0
  lookAt (Vertex3 0 0 (-10+zoom)) (Vertex3 0 0 0) (Vector3 0 1 0)
  matrixMode $= Modelview 0
  where
    w' = realToFrac w
    h' = realToFrac h

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -> IORef Double -- parameters a and b
         -> IORef Double -- isolevel
         -> IORef Double -- zoom
         -> KeyboardCallback
keyboard rot1 rot2 rot3 a b l zoom c _ =
  case c of
    'e' -> rot1 $~! subtract 1
    'r' -> rot1 $~! (+ 1)
    't' -> rot2 $~! subtract 1
    'y' -> rot2 $~! (+ 1)
    'u' -> rot3 $~! subtract 1
    'i' -> rot3 $~! (+ 1)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'f' -> a $~! (+ 0.02)
    'v' -> a $~! subtract 0.02
    'g' -> b $~! (+ 0.02)
    'b' -> b $~! subtract 0.02
    'h' -> l $~! (+ 0.1)
    'n' -> l $~! subtract 0.1
    'q' -> leaveMainLoop
    _   -> return ()

idle :: IdleCallback
idle = postRedisplay Nothing

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Goursat surface"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 0 (-100) 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  a <- newIORef (-0.27)
  b <- newIORef (-0.5)
  l <- newIORef 2.0
  displayCallback $= display rot1 rot2 rot3 a b l zoom
  reshapeCallback $= Just (resize 0)
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 a b l zoom)
  idleCallback $= Just idle
  putStrLn "*** Goursat surface ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Increase/decrease parameters:\n\
        \        f, v, g, b, h, n\n\
        \"
  mainLoop
