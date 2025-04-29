module ConfigTypes where
data Table = Table String [(String, ConfigType)] deriving Show
data ConfigType = Bool Bool | String String | Int Int deriving Show
