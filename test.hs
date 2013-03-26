module Main where

import Data.Ratio
import Text.ParserCombinators.Parsec (parse)
import STL

main = handle =<< fmap (parse stl_file "(stdin)") getContents
	where
	handle (Left msg) = putStrLn $ "Error: " ++ (show msg)
	handle (Right v) = print v
