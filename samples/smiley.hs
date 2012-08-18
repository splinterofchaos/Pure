
import Control.Applicative
import Control.Monad (unless)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

-- Simple vector type.
type Vec   = (GLfloat,GLfloat)

vec :: GLfloat -> GLfloat -> Vec
vec = (,)

(>*) :: Vec -> GLfloat -> Vec
(x,y) >* u = ( x*u, y*u )

(>+<) :: Vec -> Vec -> Vec
(x,y) >+< (u,v) = ( x+u, y+v )
(>-<) :: Vec -> Vec -> Vec
(x,y) >-< (u,v) = ( x-u, y-v )

neg :: Vec -> Vec
neg (x,y) = (-x,-y)

fromAngle :: GLfloat -> Vec
fromAngle x = (cos x, sin x)

-- OpenGL layer
vert :: Vec -> IO ()
vert = vertex . uncurry Vertex2

trans :: Vec -> IO ()
trans (x,y) = translate $ Vector3 x y 0

scalex :: GLfloat -> IO ()
scalex x = scale x x x

type Scale = IORef GLfloat
type Pos   = IORef Vec

main :: IO ()
main = do
    (progn,_) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello"  

    scale  <- newIORef (1 :: GLfloat)
    pos    <- newIORef $ vec 0 0

    vel    <- newIORef $ vec 0 0
    dscale <- newIORef (0 :: GLfloat)

    displayCallback $= display scale pos
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just (keyboardMouse dscale vel)
    idleCallback $= Just (idle scale dscale pos vel)

    mainLoop

speed  = 0.01  :: GLfloat -- Speed smiley moves.
zspeed = 0.005 :: GLfloat -- Zoom speed.

idle :: Scale -> Scale -> Pos -> Pos -> IO ()
idle scale dscale pos vel = do
    -- p = pos + vel
    p <- (>+<) <$> readIORef pos <*> readIORef vel

    (p',s) <-  (\s ds -> (p>*ds>+<p,s*ds+s)) <$> readIORef scale <*> readIORef dscale
    pos   $= p'
    scale $= s

keyboardMouse :: Scale -> Pos -> 
                 Key -> KeyState -> Modifiers -> Position -> IO () 
keyboardMouse dscale vel key state _ _ = 
    either (moveAct state vel) (zoomAct state dscale) (moveOrZoom key)

moveOrZoom :: Key -> Either Vec GLfloat
moveOrZoom (SpecialKey KeyLeft)  = Left (-1, 0)
moveOrZoom (SpecialKey KeyRight) = Left ( 1, 0)
moveOrZoom (SpecialKey KeyUp)    = Left ( 0, 1)
moveOrZoom (SpecialKey KeyDown)  = Left ( 0,-1)
moveOrZoom (Char '=') = Right zspeed
moveOrZoom (Char '-') = Right (-zspeed)
moveOrZoom _ = Left (0,0) -- (Right 0) would work just as well.

moveAct :: KeyState -> Pos -> Vec -> IO ()
moveAct Up   vel mod = modifyIORef vel (nudgeBy (neg mod))
moveAct Down vel mod = modifyIORef vel (nudgeBy mod)

nudgeBy :: Vec -> Vec -> Vec
nudgeBy dv v = dv >* speed >+< v

zoomAct :: KeyState -> Scale -> GLfloat -> IO ()
zoomAct Up   ds mod = modifyIORef ds (pure 0)
zoomAct Down ds mod = modifyIORef ds (pure mod)


reshape s@(Size w h) = viewport $= (Position 0 0,s)

display :: Scale -> Pos -> IO ()
display s p = do
    s' <- get s
    p' <- get p

    clear [ ColorBuffer ]
    draw s' p'

    swapBuffers
    postRedisplay Nothing

draw :: GLfloat -> Vec -> IO ()
draw s p = 
    unless ( fst p + s < -1 || fst p - s > 1 
          || snd p + s < -1 || snd p - s > 1 
          || s < 0.005 ) $ do
    loadIdentity
    trans  p
    scalex s
    
    let (x,y) = p
        a = 0.2 / s
    color $ Color3 (atan x/2 + 0.5) (cos a/2 + 0.5)
                                    (sin a/2 + 0.5)

    renderLoop $ facePts
    renderLoop $ smilePts 0.7 ++ reverse (smilePts 1.3) 

    let ds  = 0.45
        loff = (  ds, 0.15 )
        roff = ( -ds, 0.15 )
        s' = s * ds
        in do draw s' $ loff>*s >+< p  
              draw s' $ roff>*s >+< p

renderLoop = renderPrimitive  LineLoop . mapM_ vert

facePts = map toVec [1..steps]
    where steps = 25
          circumference = 2 * pi -- Assuming r=0.
          stepSize = circumference / steps
          toVec    = fromAngle . (* stepSize)

smilePts mod = map toVec [1..steps]
    where steps = 25
          range = 0.7 - (-0.7)
          stepSize = range / (steps+1)
          toVec i = vec x y
            where x = -0.7 + i*stepSize
                  yx x' = x' * x' * mod
                  y = yx x - yx 0.7 - 0.3
