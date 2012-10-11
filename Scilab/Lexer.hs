module Scilab.Lexer (Token (..), lexer, (~==)) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM2, void)

-- text
import qualified Data.Text as T

-- parsec
import
  Text.Parsec
  (try,
    string,
    char,
    many1,
    digit,
    letter,
    (<|>),
    many,
    noneOf,
    parse,
    ParseError,
    eof,
    option,
    oneOf)
import Text.Parsec.Text (Parser)

data Token
  = TIf
      | TThenDo
      | TElse
      | TElseIf
      | TEnd
      | TWhile
      | TFor
      | TEq
      | TDiff
      | TAttr
      | TOP
      | TCP
      | TGT
      | TGTE
      | TLT
      | TLTE
      | TColon
      | TComma
      | TAnd
      | TOr
      | TNot
      | TAdd
      | TSub
      | TMul
      | TPow
      | TDiv
      | TOSB
      | TCSB
      | TNlSc
      | TNumber Double
      | TId T.Text
      | TStr T.Text
    deriving (Show, Eq)

lexer :: T.Text -> Either ParseError [Token]
lexer = parse (many (token <* whites) <* eof) "lexer"

(~==) :: Token -> Token -> Bool
(TNumber {}) ~== (TNumber {}) = True
(TId {}) ~== (TId {}) = True
(TStr {}) ~== (TStr {}) = True
a ~== b = a == b

token :: Parser Token
token
  = whites
    >> (try (string "if" >> return TIf)
      <|> try (string "then" >> return TThenDo)
      <|> try (string "do" >> return TThenDo)
      <|> try (string "elseif" >> return TElseIf)
      <|> try (string "else" >> return TElse)
      <|> try (string "end" >> return TEnd)
      <|> try (string "while" >> return TWhile)
      <|> try (string "for" >> return TFor)
      <|> try (string "==" >> return TEq)
      <|> try (string "<>" >> return TDiff)
      <|> try (string "<=" >> return TLTE)
      <|> try (string ">=" >> return TGTE)
      <|> (char '=' >> return TAttr)
      <|> (char '(' >> return TOP)
      <|> (char ')' >> return TCP)
      <|> (char '>' >> return TGT)
      <|> (char '<' >> return TLT)
      <|> (char ':' >> return TColon)
      <|> (char ',' >> return TComma)
      <|> (char '[' >> return TOSB)
      <|> (char ']' >> return TCSB)
      <|> (char '\n' >> return TNlSc)
      <|> (char ';' >> return TNlSc)
      <|> (string "\r\n" >> return TNlSc)
      <|> (char '+' >> return TAdd)
      <|> (try (string ".+") >> return TAdd)
      <|> (char '-' >> return TSub)
      <|> (try (string ".-") >> return TSub)
      <|> (char '*' >> return TMul)
      <|> (try (string ".*") >> return TMul)
      <|> (char '/' >> return TDiv)
      <|> (try (string "./") >> return TDiv)
      <|> (char '^' >> return TPow)
      <|> (string ".^" >> return TPow)
      <|> (char '~' >> return TNot)
      <|> (char '&' >> return TAnd)
      <|> (char '|' >> return TOr)
      <|> try
        (TNumber
          <$> read
          <$> concat
          <$> sequence
            [many1 digit,
              option "" $ try $ (: []) <$> char '.',
              option "" $ try $ many1 digit])
      <|> try (TId <$> T.pack <$> liftM2 (:) fid rid)
      <|> try
        (oneOf "'\"" >> TStr <$> T.pack <$> many (noneOf "'\"") <* char '"'))

fid :: Parser Char
fid = cid <|> char '%'

rid :: Parser String
rid = many $ cid <|> digit

cid :: Parser Char
cid
  = letter
    <|> char '_'
    <|> char '#'
    <|> char '!'
    <|> char '$'
    <|> char '?'

whites :: Parser ()
whites = void $ many $ oneOf " \t"
