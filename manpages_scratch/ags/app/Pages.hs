{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pages
where

import Web.Scotty
import qualified Data.Text.Lazy as T
import qualified Data.Map.Strict as Map

scoreBoard :: [(Int, String)] -> ActionM ()
scoreBoard games = do
  let header = T.pack "<html>\
      \<head>\
        \<link rel=\"stylesheet\"\
          \href=\"https://unpkg.com/purecss@1.0.0/build/pure-min.css\"\
          \integrity=\"sha384-nn4HPE8lTHyVtfCBi5yW9d20FjT8BJwUXyWZT9InLYax14RDjBj46LmSztkmNP9w\"\
          \crossorigin=\"anonymous\">\
        \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\
      \</head>\
      \<body>\
        \<h1>Games</h1>\
        \<table class=\"pure-table pure-table-bordered\">\
          \<thead>\
            \<tr><th>Port</th><th>Map</th></tr>\
          \</thead>\
          \<tbody>\
        \"
  let footer = T.pack "</tbody>\
    \</table>\
      \</body>\
        \</html>"
  let body = map (\(p, n) -> T.pack (concat ["<tr><td><a href=\"scoreboard/port/", show p, "\">", show p, "</a></td><td>", n, "</td></th>"])) games
  html $ T.concat $ concat [[header], body, [footer]]

scoreBoardPerPort :: Int -> [(String, Map.Map String Integer)] -> ActionM ()
scoreBoardPerPort port scores = do
    let header = T.concat [T.pack ("<html>\
        \<head>\
          \<link rel=\"stylesheet\"\
            \href=\"https://unpkg.com/purecss@1.0.0/build/pure-min.css\"\
            \integrity=\"sha384-nn4HPE8lTHyVtfCBi5yW9d20FjT8BJwUXyWZT9InLYax14RDjBj46LmSztkmNP9w\"\
            \crossorigin=\"anonymous\">\
          \<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\
        \</head>\
        \<body>\
          \<h1>Scores for port "), T.pack(show port), (T.pack "</h1>")]

    let footer = T.pack "</body></html>"
    let body = map singleScore scores
    html $ T.concat $ concat [[header], body, [footer]]

singleScore :: (String, Map.Map String Integer) -> T.Text
singleScore (title, m) = T.concat ["<h2>", T.pack title, "</h2>\
\<table class=\"pure-table pure-table-bordered\">\
  \<thead>\
    \<tr><th>Punter</th><th>Score</th></tr>\
  \</thead>\
  \<tbody>", rows m, T.pack "</tbody></table>"]

rows :: Map.Map String Integer -> T.Text
rows rs =
  let
    tups = Map.toList rs
    in T.concat (map (\(p, s) -> T.concat["<tr><th>", T.pack p, "</th><th>", T.pack (show s),"</th></tr>"]) tups)
