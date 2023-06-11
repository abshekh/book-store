{-# LANGUAGE OverloadedStrings #-}

module Repository.BookRepository
  ( getAllBooks
  , getBook
  , upsertBook
  , deleteBook
  ) where

import Data.Maybe (fromJust)
import qualified Data.UUID.V4 as UUID
import Database.SQLite.Simple
import Models.Book as B

getAllBooks :: FilePath -> IO [Book]
getAllBooks dbFile = do
  xs <- withConnection dbFile $ \conn -> query_ conn "SELECT * FROM books"
  return $ map (\(_id, _name, _author) -> Book _id _name _author) xs

getBook :: FilePath -> String -> IO (Maybe Book)
getBook dbFile _id = do
  xs <-
    withConnection dbFile $ \conn ->
      query conn "SELECT * FROM books where id = ?" (Only _id)
  return . headMaybe $ map (\(_id, _name, _author) -> Book _id _name _author) xs
  where
    headMaybe [] = Nothing
    headMaybe (book:_) = Just book

upsertBook :: FilePath -> Book -> IO Book
upsertBook dbFile book = do
  upsertBookHelper (B.id book)
  where
    upsertBookHelper (Just _) = do
      withConnection dbFile $ \conn ->
        execute
          conn
          "UPDATE books set name = ?, author = ? where id = ?"
          [B.name book, B.author book, fromJust (B.id book)]
      return book
    upsertBookHelper (Nothing) = do
      uuid <- UUID.nextRandom
      let book' = book {B.id = Just $ show uuid}
      withConnection dbFile $ \conn ->
        execute
          conn
          "INSERT INTO books VALUES (?, ?, ?)"
          [fromJust (B.id book'), B.name book', B.author book']
      return book'

deleteBook :: FilePath -> String -> IO ()
deleteBook dbFile id = do
  withConnection dbFile $ \conn ->
    execute
      conn
      "DELETE FROM books where id = ?" (Only id)
