{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Main
  ( main
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Database.SQLite.Simple
import Models.Book
import Network.Wai.Handler.Warp
import qualified Repository.BookRepository as BR
import Servant
import Servant.API.Generic (Generic)

type API = NamedRoutes Routes
data Routes mode =
  Routes
    { _getAllBooks :: mode :- "books" :> Get '[ JSON] [Book]
    , _getBook :: mode :- "books" :> Capture "id" String :> Get '[ JSON] Book
    , _postBook :: mode :- "books" :> ReqBody '[ JSON] Book :> Post '[ JSON] Book
    , _deleteBook :: mode :- "books" :> Capture "id" String :> Delete '[ JSON] NoContent
    }
  deriving (Generic)

app :: Application
app = serve (Proxy @API) server
  where
    server =
      Routes
        { _getAllBooks = getAllBooks
        , _getBook = getBook
        , _postBook = postBook
        , _deleteBook = deleteBook
        }

startApp :: IO ()
startApp = run 8080 app

initDB :: FilePath -> IO ()
initDB dbfile =
  withConnection dbfile $ \conn ->
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS books (id text NOT NULL, name text NOT NULL, author text NOT NULL, PRIMARY KEY (id))"

getDBFile :: FilePath
getDBFile = "resources/test.db"

getBook :: String -> Handler Book
getBook id' = do
  book <- liftIO $ BR.getBook getDBFile id'
  if isJust book
    then return $ fromJust book
    else error "Book not found"

getAllBooks :: Handler [Book]
getAllBooks = liftIO $ BR.getAllBooks getDBFile

postBook :: Book -> Handler Book
postBook book = liftIO $ BR.upsertBook getDBFile book

deleteBook :: String -> Handler NoContent
deleteBook id' = liftIO $ BR.deleteBook getDBFile id' >> return NoContent

main :: IO ()
main = initDB getDBFile >> startApp
