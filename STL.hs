module STL (stl_file, Triangle(Triangle), Vector(Vector)) where

import Text.ParserCombinators.Parsec (between, string, many, char, letter, digit, manyTill, spaces, space, GenParser, getInput, setInput)
import Numeric (readSigned, readFloat)

import Control.Applicative ((<*>), (*>), (<*), (<$>), (<|>), empty, (<$))

data Triangle a = Triangle {_normal :: (Vector a), _v1 :: (Vector a), _v2 :: (Vector a), _v3 :: (Vector a)} deriving (Eq, Show)

data Vector a = Vector a a a deriving (Eq, Show)

name_char = letter <|> digit <|> char '_' <|> char '-' <|> char '.'
with_name tag = (token $ string tag) *> (token $ manyTill name_char space)

read_num :: (RealFrac a) => GenParser Char st a
read_num = fmap (readSigned readFloat) getInput >>= read_input
	where
	read_input [(n, s')] = n <$ setInput s'
	read_input _ = empty

token = between spaces spaces

stl_file :: (RealFrac a) => GenParser Char st [Triangle a]
stl_file = between (with_name "solid ") (with_name "endsolid ") (many facet)

facet :: (RealFrac a) => GenParser Char st (Triangle a)
facet = between (token $ string "facet") (token $ string "endfacet") inner_facet

vect name = Vector <$> ((token $ string name) *> (token $ read_num)) <*> (token $ read_num) <*> (token $ read_num)

inner_facet :: (RealFrac a) => GenParser Char st (Triangle a)
inner_facet = Triangle <$> (vect "normal") <* (token $ string "outer") <* (token $ string "loop") <*> vect "vertex" <*> vect "vertex" <*> vect "vertex" <* (token $ string "endloop")
