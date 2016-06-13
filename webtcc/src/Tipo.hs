{-# LANGUAGE TemplateHaskell #-}

module Tipo where
 
import Database.Persist.TH
import Prelude

data Tipo =  Adm | Comum deriving (Show,Read)

derivePersistField "Tipo"