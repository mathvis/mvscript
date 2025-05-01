module ConfigTypes where
import Data.Map

data Table = Table String (Map String ConfigType) deriving Show
data ConfigType = Bool Bool | String String | Int Int deriving Show
