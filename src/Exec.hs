module Exec where

import           Types


-- |this is the main executable of our app.
--  it retrieves the relevant data from a database, performs the solution
--  optimization, and releases the result, by saving it to the DB and
--  printing it to the screen.
app :: IO ()
app = putStrLn "Hello World!"
