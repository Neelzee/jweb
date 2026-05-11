module Whitelist where

import Data.Text (Text)

entries :: [(Text, Text)]
entries =
  [ ("nilsien2001@gmail.com", "Nils Michael")
  , ("jnnesh@gmail.com", "Janne")
  ]

member :: Text -> Bool
member email = any ((== email) . fst) entries

lookupName :: Text -> Maybe Text
lookupName email = lookup email entries
