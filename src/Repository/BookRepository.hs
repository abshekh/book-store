{-# LANGUAGE OverloadedStrings #-}

module Repository.BookRepository
  ( getAllBooks
  , addBook
  ) where

import Database.SQLite.Simple
import Models.Book as B
import qualified Data.UUID.V4 as UUID
import Data.Maybe (fromJust)

getAllBooks :: FilePath -> IO [Book]
getAllBooks dbFile = do
  x <- withConnection dbFile $ \conn -> query_ conn "SELECT * FROM books"
  return $ map (\(_id, _name, _author) -> Book _id _name _author) x

addBook :: FilePath -> Book -> IO Book
addBook dbFile book' = do
  uuid <- UUID.nextRandom
  let book = book' { B.id = Just $ show uuid }
  withConnection dbFile $ \conn ->
    execute
      conn
      "INSERT INTO books VALUES (?, ?, ?)"
      [fromJust (B.id book), B.name book, B.author book]
  return book
