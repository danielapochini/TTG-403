{-# LANGUAGE TemplateHaskell #-}

module Tipo where
 
import Database.Persist.TH
import Prelude

data Tipo =  Administrador | Funcionario deriving (Show, Eq, Read)

derivePersistField "Tipo"