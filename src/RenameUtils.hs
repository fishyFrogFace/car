module RenameUtils ( carFieldRename ) where

carFieldRename :: String -> String
carFieldRename "carType" = "type"
carFieldRename name      = name

