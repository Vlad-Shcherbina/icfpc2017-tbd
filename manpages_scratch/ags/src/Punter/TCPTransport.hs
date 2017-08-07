module Punter.TCPTransport where

import qualified Data.Map.Strict as Map
import Punter.Database

type PortNo = Int

spawn :: Int -> PortNo -> String -> IO ()
spawn qty port mapName = do
  putStrLn $
    concat ["Started a game. Punter quantity: ", show qty, ", port: ", show port, ", map: ", mapName ]
  saveScore port (GameScore (Map.fromList [("1", -43), ("9", 54)]))
