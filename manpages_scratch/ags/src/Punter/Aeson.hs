module Punter.Aeson where

import           Data.Aeson
import           Data.Aeson.Types

opts :: Options
opts = defaultOptions { fieldLabelModifier = drop 2}
