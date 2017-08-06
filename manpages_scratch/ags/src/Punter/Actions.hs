{-# LANGUAGE NamedFieldPuns #-}
module Punter.Actions where

import           Data.Foldable
import qualified Data.Map      as M
import           Data.Maybe
import           Punter.Game
import           Punter.Map

claim :: Integer -> Game -> River -> Game
claim p g@(Game {g_players, g_free, g_moves}) river =
    claimIfFree $ isJust $ find ((==) river) g_free
  where
    claimIfFree False = pass p g
    claimIfFree True = g { g_players = M.update claimDo p g_players
                         , g_moves   = g_moves ++ [claimingMove p river]
                         , g_free    = filter ((/=) river) g_free }
    claimDo pr@(Punter {claimed, splurges}) = Just $
        pr { claimed = (river:claimed)
           , splurges = unsplurge splurges 1 }

pass :: Integer -> Game -> Game
pass p g@(Game {g_moves}) = g { g_moves = g_moves ++ [passingMove p] }

splurge :: Integer -> [Integer] -> Game -> Game
splurge p route (g@Game {g_free}) =
    splurgeIfFree $ isJust $ toRivers route g_free
  where
    splurgeIfFree = undefined

toRivers :: [Integer] -> [River] -> Maybe [River]
toRivers xs rivers = toRiversDo xs rivers (Just [])
  where
    toRiversDo _ _ Nothing = Nothing
    toRiversDo [] _ _ = Nothing
    toRiversDo [x] _ acc = acc
    toRiversDo (x:y:xs) rivers1 (Just acc) =
        let maybeRiver =
                oneOfTwo (find ((==) $ River x y) rivers)
                         (find ((==) $ River y x) rivers)
            rivers2 = delete' maybeRiver rivers1
        in
          toRiversDo (y:xs) rivers2 $ (Just acc) `shove` maybeRiver


shove :: Maybe [a] -> Maybe a -> Maybe [a]
shove Nothing _ = Nothing
shove _ Nothing = Nothing
shove (Just xs) (Just y) = Just (xs ++ [y])

oneOfTwo :: Maybe a -> Maybe a -> Maybe a
oneOfTwo (Just x) _ = Just x
oneOfTwo _ (Just y) = Just y
oneOfTwo _ _        = Nothing

delete' :: (Eq a) => Maybe a -> [a] -> [a]
delete' Nothing x = x
delete' (Just y) x = filter ((/=) y) x

flush :: Game -> Game
flush g = g { g_moves = [] }

rays :: [Mine] -> [River] -> M.Map Mine [(Site, Integer)]
rays allMines someRivers = undefined

unsplurge :: Integer -> Integer -> Integer
unsplurge 0 _ = 0
unsplurge x y
    | x >= y = x - y
    | otherwise = error "Sploosh"
