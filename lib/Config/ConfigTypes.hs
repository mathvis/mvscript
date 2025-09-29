module Config.ConfigTypes (module Config.ConfigTypes) where
import Data.Map

type Options = Map String ConfigType
data Table = Table String Options deriving Show
data ConfigType = Bool Bool | String String | Int Int deriving Show
data ConfigTypeName = BoolT | StringT | IntT deriving Eq

type ParsedConfig = [Table]
type FlatParsedConfig = Options
