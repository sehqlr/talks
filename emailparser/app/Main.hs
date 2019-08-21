module Main where
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

at = char '@'

inAlnum = inClass "a-z0-9-"

inAlnumHyphen = inClass "a-z0-9-"

inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

dotSep p = p `sepBy` (char '.')

hyphenSepAlnum = inAlnum `sepBy` (char '-')

ip = do
  count 3 $ octet <*> dot
  octet <|> group
  where
    group = hyphenSepAlnum <*> char ':' <*> many1 $ notInClass "\n\r\t \\" <|> (char '\\' <*> notInClass "\n\r")
    octet = decimal >>= limitSize
    limitSize i =
      if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i

emailValidator :: Parser T.Text
emailValidator = do
  many $ dotSep inGrabBag <|> (char '"') <*> many $ notInClass "\n\r\t \\\"" <*> char '"'
  <*> at
  <*> many $ dotSep <$> hyphenSepAlnum <|> do
						char '['
                                                ip
                                                char ']'

main :: IO ()
main = do
  addrs <- TIO.readFile "./addresses.txt"
  parseTest emailValidator addrs
