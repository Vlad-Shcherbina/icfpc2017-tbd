{-# LANGUAGE NamedFieldPuns #-}
module Punter.Actions where

import           Data.Either
import           Data.Foldable
import qualified Data.Map      as M
import           Data.Maybe
import           Prelude       hiding (id)
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

riversFrom :: Integer -> [River] -> [River]
riversFrom v = filter (\River{source,target} -> (v == source) || (v == target))

adjacent :: Integer -> [River] -> [Integer]
adjacent v = map
             (\River{source,target} -> if (source == v) then target else source)
             . (riversFrom v)

front' :: [River] -> [Integer] -> [Integer]
front' rs = concat . (map (flip adjacent rs))

front :: [Integer] -> [River] -> [Integer]
front = flip front'

rays :: Integer -> [River] -> M.Map Integer Integer
rays v rs = raysDo (front [v] rs) ( 1, (M.fromList [(v, 0)]) )
  where
    raysDo ::
        [Integer] -> (Integer, M.Map Integer Integer) -> M.Map Integer Integer
    raysDo [] (_, sigma) = sigma
    raysDo delta (depth, sigma) =
        raysDo (front delta rs)
               (depth + 1, (foldl (\s1 v1 -> ins v1 depth s1) sigma delta))

score :: Game -> Game
score = undefined

tuck :: a -> b -> (b, a)
tuck = flip (,)

ins :: (Ord b) => b -> a -> M.Map b a -> M.Map b a
ins = M.insertWith (\_ y -> y)

filterRight :: [Either a b] -> [b]
filterRight xs = map fromRight' (filter isRight xs)

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' _ = error "Can you tell what is right and what is left?"

unsplurge :: Integer -> Integer -> Integer
unsplurge 0 _ = 0
unsplurge x y
    | x >= y = x - y
    | otherwise = error "Sploosh"  -- Walking the thin ice

toi :: Int -> Integer
toi = toInteger
