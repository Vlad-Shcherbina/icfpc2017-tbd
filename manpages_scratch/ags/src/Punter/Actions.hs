module Punter.Actions where

import           Data.Map    as M
import           Punter.Game
import           Punter.Map

claim :: Integer -> Game -> River -> Game
claim p g river = undefined

pass :: Integer -> Game -> Game
pass p g = undefined

splurge :: Integer -> [Integer] -> Game -> Game
splurge p route g = undefined

flush :: Game -> Game
flush g = g { moves = [] }

rays :: [Mine] -> [River] -> M.Map Mine [(Site, Integer)]
rays allMines someRivers = undefined
