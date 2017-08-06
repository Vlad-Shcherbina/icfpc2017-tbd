{-# LANGUAGE NamedFieldPuns #-}
module Punter.Actions where

import           Data.Foldable
import qualified Data.Map      as M
import           Data.Maybe
import           Punter.Game
import           Punter.Map

claim :: Integer -> Game -> River -> Game
claim p g@(Game {players, free, moves}) river =
    claimIfFree $ isJust $ find ((==) river) free
    where
      claimIfFree False = pass p g
      claimIfFree True = g { players = M.update claimDo p players
                           , moves   = moves ++ (claimingMove p river:moves)
                           , free    = filter ((/=) river) free }
      claimDo pr@(Punter {claimed, splurges}) = Just $
          pr { claimed = (river:claimed)
             , splurges = unsplurge splurges 1 }

pass :: Integer -> Game -> Game
pass p g = undefined

splurge :: Integer -> [Integer] -> Game -> Game
splurge p route g = undefined

flush :: Game -> Game
flush g = g { moves = [] }

rays :: [Mine] -> [River] -> M.Map Mine [(Site, Integer)]
rays allMines someRivers = undefined

unsplurge :: Integer -> Integer -> Integer
unsplurge 0 _ = 0
unsplurge x y
    | x >= y = x - y
    | otherwise = error "Sploosh"
