module Punter.TCPTransport where

type PuntersQty = Int
type PortNo = Int

spawn :: PuntersQty -> PortNo -> String -> IO ()
spawn qty port mapName =
  putStrLn $
    concat ["Started a game. Punter quantity: ", show qty, ", port: ", show port, ", map: ", mapName ]
