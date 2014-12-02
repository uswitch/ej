{-# LANGUAGE OverloadedStrings #-}

module EDN where

import Prelude hiding (map)
import Control.Applicative
import Data.Ratio
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.ByteString.Char8 as AC8
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data Value
  = Nil
  | Boolean Bool
  | String T.Text
  | Character Char
  | Symbol T.Text
  | Keyword T.Text
  | WholeNumber Integer
  | RationalNumber Rational
  | RealNumber Double
  | List [Value]
  | Vector [Value]
  | Map [(Value, Value)]
  | Set [Value]

isWs :: Char -> Bool
isWs ' ' = True
isWs ',' = True
isWs '\n' = True
isWs '\r' = True
isWs '\t' = True
isWs _ = False

isPunc :: Char -> Bool
isPunc '!' = True
isPunc '$' = True
isPunc '%' = True
isPunc '&' = True
isPunc '*' = True
isPunc '+' = True
isPunc '-' = True
isPunc '.' = True
isPunc '/' = True
isPunc '=' = True
isPunc '?' = True
isPunc '_' = True
isPunc _ = False

ws :: A.Parser ()
ws = AC8.skipWhile isWs

comment :: A.Parser ()
comment = ws >> AC8.char ';' >> AC8.skipWhile (/= '\n')

discard :: A.Parser ()
discard = () <$ (ws >> AC8.string "#_" >> value)

symbolHead :: Char -> Bool
symbolHead c = AC8.isAlpha_ascii c || isPunc c

symbolBody :: Char -> Bool
symbolBody c = AC8.isAlpha_ascii c || isPunc c || AC8.isDigit c || c == '#' || c == ':'

tag :: A.Parser ()
tag = ws >> AC8.char '#' >> AC8.satisfy AC8.isAlpha_ascii >> AC8.skipWhile symbolBody

nil :: A.Parser Value
nil = ws >> Nil <$ AC8.string "nil"

boolean :: A.Parser Value
boolean = ws >> Boolean <$> (True <$ AC8.string "true" <|> False <$ AC8.string "false")

string :: A.Parser Value
string = ws >> String . TE.decodeUtf8 . BS.pack <$> (AC8.char '"' *> many (ch <|> esc) <* AC8.char '"')
  where ch = AC8.satisfy (\c -> c /= '"' && c /= '\\')
        esc = AC8.char '\\' >> (dq <|> nl <|> ret <|> tab)
        dq = '"' <$ AC8.char '"'
        nl = '\n' <$ AC8.char 'n'
        ret = '\r' <$ AC8.char 'r'
        tab = '\t' <$ AC8.char 't'

character :: A.Parser Value
character = ws >> Character <$> (AC8.char '\\' *> (nl <|> ret <|> spc <|> tab <|> ch))
   where nl = '\n' <$ AC8.string "newline"
         ret = '\r' <$ AC8.string "return"
         spc = ' ' <$ AC8.string "space"
         tab = '\t' <$ AC8.string "tab"
         ch = AC8.satisfy (not . isWs)
         
sym :: A.Parser T.Text
sym = do
    c <- AC8.satisfy symbolHead
    x <- AC8.takeWhile symbolBody
    return $ TE.decodeUtf8 (BS.cons c x)

symbol :: A.Parser Value
symbol = ws >> Symbol <$> sym

keyword :: A.Parser Value
keyword = ws >> Keyword <$> (AC8.char ':' >> sym)

number :: A.Parser Value
number = ws >> do
  n <- AC8.number <* AC8.skipWhile (\c -> c == 'M' || c == 'N') -- ugly
  case n of
    AC8.I i -> return $ WholeNumber i
    AC8.D d -> return $ RealNumber d

rational :: A.Parser Value
rational = ws >> RationalNumber <$> (do n <- AC8.signed AC8.decimal
                                        _ <- AC8.char '/'
                                        d <- AC8.decimal
                                        return $ n % d)

list :: A.Parser Value
list = ws >> List <$> (AC8.char '(' *> many value <* (ws >> AC8.char ')'))

vector :: A.Parser Value
vector = ws >> Vector <$> (AC8.char '[' *> many value <* (ws >> AC8.char ']'))

map :: A.Parser Value
map = ws >> Map <$> (AC8.char '{' *> many ((,) <$> value <*> value) <* (ws >> AC8.char '}'))

set :: A.Parser Value
set = ws >> Set <$> (AC8.string "#{" *> many value <* (ws >> AC8.char '}'))

value  :: A.Parser Value
value = A.skipMany (comment <|> discard <|> tag) >> 
        nil <|> boolean <|> string <|> character <|> symbol <|> keyword <|> rational <|> number <|>
        list <|> vector <|> map <|> set
