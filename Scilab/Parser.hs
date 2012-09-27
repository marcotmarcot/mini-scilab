module Scilab.Parser (parser, Command (..), Reference (..), Expr (..)) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM2)

-- text
import qualified Data.Text as T

-- transformers
import Data.Functor.Identity (Identity)

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
import
  Text.Parsec.Expr
  (buildExpressionParser,
    OperatorTable,
    Assoc (AssocNone, AssocLeft, AssocRight),
    Operator (Infix, Prefix))

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
expr = buildExpressionParser expr_table noop_expr

expr_table :: OperatorTable [Token] () Identity Expr
expr_table
  = [
    [bin TPow EPow AssocRight],
    [binl TMul EMul, binl TDiv EDiv],
    [binl TAdd EAdd, binl TSub ESub],
    [bin TEq EEq AssocNone,
      bin TDiff EDiff AssocNone,
      bin TGT EGT AssocNone,
      bin TGTE EGTE AssocNone,
      bin TLT ELT AssocNone,
      bin TLTE ELTE AssocNone],
    [Prefix $ token TNot >> return ENot],
    [binl TAnd EAnd],
    [binl TOr EOr]]

binl :: Token -> (Expr -> Expr -> Expr) -> Operator [Token] () Identity Expr
binl tk cons = bin tk cons AssocLeft

bin
  :: Token
    -> (Expr -> Expr -> Expr)
    -> Assoc
    -> Operator [Token] () Identity Expr
bin tk cons = Infix $ token tk >> return cons

noop_expr :: Parser Expr
noop_expr
  = literal_expr
    <|> vec_expr
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
