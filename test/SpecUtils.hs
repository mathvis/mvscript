module SpecUtils (dummyPos) where

import Text.Megaparsec (SourcePos)
import Text.Megaparsec.Pos (initialPos)

dummyPos :: SourcePos
dummyPos = initialPos ""
