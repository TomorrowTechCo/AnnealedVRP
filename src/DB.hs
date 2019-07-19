{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module DB where

import           Database.Persist

share [] [persistLowerCase|

|]
