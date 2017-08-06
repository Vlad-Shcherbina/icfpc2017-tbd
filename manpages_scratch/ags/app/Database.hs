module Database
where

import qualified Data.Text.Lazy as T

type GameType = T.Text
type Key = T.Text

portForGame :: GameType -> Key -> IO Int
portForGame _ _ = return 4444
