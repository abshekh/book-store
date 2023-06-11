{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Prelude hiding (id)

import Control.Monad.IO.Class
import Database.SQLite.Simple
import Models.Book
import Network.Wai.Handler.Warp
import Repository.BookRepository
import Servant

type API
   = "books" :> Get '[ JSON] [Book] :<|> "books/add" :> ReqBody '[ JSON] Book :> Post '[ JSON] Book

server :: Server API
server = getBooks :<|> postBook

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

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

postBook :: Book -> Handler Book
postBook book = liftIO $ addBook getDBFile book

getBooks :: Handler [Book]
getBooks = liftIO $ getAllBooks $ getDBFile

main :: IO ()
main = initDB getDBFile >> startApp
