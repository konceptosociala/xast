module Xast.Utils.Pretty where

data Colored = Colored
   { text   :: String
   , color  :: Color
   , isBold :: Bool
   }

data Color
   = Black
   | Red
   | Green
   | Yellow
   | Blue
   | Magenta
   | Cyan
   | White

instance Show Colored where
   show :: Colored -> String
   show (Colored text color isBold) =
      let colorCode = case color of
            Black   -> "30"
            Red     -> "31"
            Green   -> "32"
            Yellow  -> "33"
            Blue    -> "34"
            Magenta -> "35"
            Cyan    -> "36"
            White   -> "37"
          boldCode = if isBold then "1" else "0"
      in "\ESC[" ++ boldCode ++ ";" ++ colorCode ++ "m" ++ text ++ "\ESC[0m"

class ToColored a where
   toColored :: a -> Colored

instance ToColored Colored where
   toColored :: Colored -> Colored
   toColored = id

instance ToColored String where
   toColored :: String -> Colored
   toColored x = Colored x White False

bold :: ToColored a => a -> Colored
bold x = let c = toColored x in c { isBold = True }

red :: ToColored a => a -> Colored
red x = let c = toColored x in c { color = Red }

green :: ToColored a => a -> Colored
green x = let c = toColored x in c { color = Green }

yellow :: ToColored a => a -> Colored
yellow x = let c = toColored x in c { color = Yellow }

blue :: ToColored a => a -> Colored
blue x = let c = toColored x in c { color = Blue }

magenta :: ToColored a => a -> Colored
magenta x = let c = toColored x in c { color = Magenta }

cyan :: ToColored a => a -> Colored
cyan x = let c = toColored x in c { color = Cyan }

white :: ToColored a => a -> Colored
white x = let c = toColored x in c { color = White }

black :: ToColored a => a -> Colored
black x = let c = toColored x in c { color = Black }