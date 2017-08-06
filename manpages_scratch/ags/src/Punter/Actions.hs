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
    claimIfFree True = g { g_players = M.update (claimDo [river]) p g_players
                         , g_moves   = g_moves ++ [claimingMove p river]
                         , g_free    = filter ((/=) river) g_free }

claimDo :: [River] -> Punter -> Maybe Punter
claimDo rs pr@(Punter {claimed, splurges}) = Just $
    pr { claimed = claimed ++ rs
       , splurges = unsplurge splurges $ toi $ length rs }

pass :: Integer -> Game -> Game
pass p g@(Game {g_moves, g_players}) =
    g { g_moves = g_moves ++ [passingMove p]
      , g_players = M.update
                    (\u@Punter{splurges} -> Just $ u {splurges = splurges + 1})
                    p
                    g_players }

pass' :: Integer -> Game -> Game
pass' p g@(Game {g_moves}) = g { g_moves = g_moves ++ [passingMove p] }

splurge :: Integer -> [Integer] -> Game -> Game
splurge p route (g@Game {g_free, g_players, g_moves}) =
    withEnoughCredit (splurges <$> (M.lookup p g_players)) $
    splurgeIfFree $ toRivers route g_free
  where
    splurgeIfFree Nothing = pass p g
    splurgeIfFree (Just (toClaim, free1)) =
        g { g_players = M.update (claimDo toClaim) p g_players
          , g_moves   = g_moves ++ [splurgingMove p route ]
          , g_free    = free1 }
    withEnoughCredit (Just credit) f
        | credit >= toi ((length route) - 1) = f
        | otherwise = pass p g
    withEnoughCredit Nothing _ = pass p g

toRivers :: [Integer] -> [River] -> Maybe ([River], [River])
toRivers route rivers = toRiversDo route rivers (Just [])
  where
    toRiversDo _ _ Nothing = Nothing
    toRiversDo [] _ _ = Nothing
    toRiversDo [_x] free1 (Just acc) = Just (free1, acc)
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
    | otherwise = error "Sploosh"  -- Walking the thin ice

toi = toInteger
