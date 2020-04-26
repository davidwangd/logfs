module Utils
(
    processPath
) where

import Data.List.Split

processPath :: FilePath -> [FilePath]
processPath = splitOn "/"