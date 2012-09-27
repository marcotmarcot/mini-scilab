module Scilab.Parser (parser, Command (..), Reference (..), Expr (..)) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM2)

-- text
import qualified Data.Text as T

-- parsec
import
  Text.Parsec
  (ParseError,
    Parsec,
    tokenPrim,
    incSourceLine,
    try,
    (<|>),
    many,
    eof,
    parse,
    many1,
    sepEndBy)

-- scilab
import Scilab.Lexer

parser :: T.Text -> [Command]
parser t = either (error . show) id $ lexer t >>= doParser

doParser :: [Token] -> Either ParseError [Command]
doParser = parse (sepEndBy command (many1 $ token TNlSc) <* eof) "parser"

type Parser = Parsec [Token] ()

command :: Parser Command
command
  = try attribution

data Command
  = CAttr Reference Expr
    deriving (Show, Eq)

data Reference = RVar T.Text | RVI T.Text Expr deriving (Show, Eq)

attribution :: Parser Command
attribution = liftM2 CAttr (call_expr RVar RVI) $ token TAttr >> expr

data Expr
  = EVar T.Text
      | EVec [Expr]
      | ECall T.Text Expr
      | EAdd Expr Expr
      | ESub Expr Expr
      | EMul Expr Expr
      | EDiv Expr Expr
      | EPow Expr Expr
      | EEq Expr Expr
      | EDiff Expr Expr
      | EGT Expr Expr
      | EGTE Expr Expr
      | ELT Expr Expr
      | ELTE Expr Expr
      | EAnd Expr Expr
      | EOr Expr Expr
      | ENot Expr
      | ENumber Double
      | EStr T.Text
    deriving (Show, Eq)

expr :: Parser Expr
expr = binOp and_expr [(TOr, EOr)]

and_expr :: Parser Expr
and_expr = binOp not_expr [(TAnd, EAnd)]

not_expr :: Parser Expr
not_expr
  = (token TNot >> ENot <$> cmp_expr)
    <|> cmp_expr

cmp_expr :: Parser Expr
cmp_expr
  = binOp
    add_expr
    [(TEq, EEq),
      (TDiff, EDiff),
      (TGT, EGT),
      (TGTE, EGTE),
      (TLT, ELT),
      (TLTE, ELTE)]

add_expr :: Parser Expr
add_expr = binOp mul_expr [(TAdd, EAdd), (TSub, ESub)]

mul_expr :: Parser Expr
mul_expr = binOp pow_expr [(TMul, EMul), (TDiv, EDiv)]

pow_expr :: Parser Expr
pow_expr = binOp noop_expr [(TPow, EPow)]

noop_expr :: Parser Expr
noop_expr
  = literal_expr
    <|> vec_expr
    <|> paren_expr
    <|> call_expr EVar ECall

literal_expr :: Parser Expr
literal_expr
  = literal_num
    <|> literal_str

literal_num :: Parser Expr
literal_num
  = (token TSub >> ENumber <$> negate <$> number)
    <|> (ENumber <$> number)

literal_str :: Parser Expr
literal_str
  = do
    (TStr s) <- token $ TStr ""
    return $ EStr s

vec_expr :: Parser Expr
vec_expr = token TOSB >> EVec <$> many expr <* token TCSB

call_expr :: (T.Text -> a) -> (T.Text -> Expr -> a) -> Parser a
call_expr cvar ccall = try (liftM2 ccall iden paren_expr) <|> cvar <$> iden

paren_expr :: Parser Expr
paren_expr = token TOP >> expr <* token TCP

binOp :: Parser Expr -> [(Token, Expr -> Expr -> Expr)] -> Parser Expr
binOp next rules
  = try
      (do
        e1 <- next
        cons <- foldr1 (<|>) $ map rule rules
        e2 <- binOp next rules
        return $ cons e1 e2)
    <|> next

rule :: (Token, Expr -> Expr -> Expr) -> Parser (Expr -> Expr ->  Expr)
rule (tk, cons) = token tk >> return cons

iden :: Parser T.Text
iden
  = do
    (TId n) <- token $ TId ""
    return n

number :: Parser Double
number
  = do
    (TNumber n) <- token $ TNumber 0
    return n

token :: Token -> Parser Token
token t
  = tokenPrim
    show
    (\s _ _ -> incSourceLine s 1)
    (\x -> if x ~== t then Just x else Nothing)
