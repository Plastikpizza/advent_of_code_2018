import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Interact as I
import Debug.Trace (trace)

type Vec2D = (Int, Int)
type World = [Particle]
data Particle = Particle {pos::Vec2D, dir::Vec2D} deriving (Show, Eq)

lineToParticle l = Particle (a,b) (c, d)
    where
        [a,b,c,d] = map read $ words $ filter (\x-> elem x "0987654123- ") l

add (a,b) (c,d) = (a+c,b+d)
scl n (a, b)  = (n*a,n*b)

fly :: Int -> Particle -> Particle
fly n p = p{pos=add (pos p) (scl n (dir p))}

window = (InWindow "Day09" (800, 600) (10, 10))

displayW :: World -> Picture
displayW particles = pictures [translate (fromIntegral x) (fromIntegral (-y)) $ circle 1 | (Particle (x,y) _)<-particles] 

handleKeys :: I.Event -> World -> World
handleKeys (I.EventKey (I.Char 'k') I.Down _ _) world = trace (show $ head world) map (fly 100) world
handleKeys (I.EventKey (I.Char 'j') I.Down _ _) world = trace (show $ head world) map (fly 50) world
handleKeys (I.EventKey (I.Char 'h') I.Down _ _) world = trace (show $ head world) map (fly 1) world
handleKeys (I.EventKey (I.Char 'g') I.Down _ _) world = trace (show $ head world) map (fly (-1)) world
handleKeys (I.EventKey (I.Char 'f') I.Down _ _) world = trace (show $ head world) map (fly (-50)) world
handleKeys (I.EventKey (I.Char 'd') I.Down _ _) world = trace (show $ head world) map (fly (-100)) world
handleKeys (I.EventKey (I.MouseButton I.WheelDown) _ _ _) world = trace (show $ head world) map (fly 1) world
handleKeys (I.EventKey (I.MouseButton I.WheelUp)   _ _ _) world = trace (show $ head world) map (fly (-1)) world
handleKeys _ w = w

handleTime :: Float -> World -> World
handleTime _ w = w

main = do
    particles <- fmap (map lineToParticle . lines) getContents
    play window white 60 particles displayW handleKeys handleTime
    -- (192,200) = (-42528,  42920) + s * (4, -4)?
    -- s+1 = 10681