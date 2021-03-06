module Scilab.Parser (parser, Command (..), Reference (..), Expr (..)) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM2, liftM3)

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
    sepEndBy,
    optional,
    option,
    sepBy)
import
  Text.Parsec.Expr
  (buildExpressionParser,
    OperatorTable,
    Assoc (AssocNone, AssocLeft, AssocRight),
    Operator (Infix, Prefix))

-- scilab
import Scilab.Lexer

data Command
  = CIf Expr [Command] [Command]
    | CAttr Reference Expr
    | CExpr Expr
    | CWhile Expr [Command]
    | CFor T.Text Expr [Command]
    deriving (Show, Eq)

parser :: T.Text -> [Command]
parser t = either (error . show) id $ lexer t >>= doParser

doParser :: [Token] -> Either ParseError [Command]
doParser = parse (commands <* eof) "parser"

type Parser = Parsec [Token] ()

commands :: Parser [Command]
commands = many (token TNlSc) >> sepEndBy command (many1 $ token TNlSc)

command :: Parser Command
command
  = ifelse
    <|> while
    <|> for
    <|> try attribution
    <|> CExpr <$> expr

ifelse :: Parser Command
ifelse = token TIf >> ifbase

ifbase :: Parser Command
ifbase
  = liftM3
    CIf
    expr
    (optional (token TThenDo) >> commands)
    (option
        []
        ((token TElse >> commands)
          <|> (token TElseIf >> (: []) <$> ifbase))
      <* token TEnd)

while :: Parser Command
while
  = liftM2
    CWhile
    (token TWhile >> expr)
    (optional (token TThenDo) >> commands <* token TEnd)

for :: Parser Command
for
  = liftM3
    CFor
    (token TFor >> iden)
    (token TAttr >> expr)
    (optional (token TThenDo) >> commands <* token TEnd)

data Reference = RVar T.Text | RVI T.Text [Expr] deriving (Show, Eq)

attribution :: Parser Command
attribution = liftM2 CAttr (call_expr RVar RVI) $ token TAttr >> expr

data Expr
  = EVar T.Text
      | EVec [Expr]
      | EVecFromTo Expr Expr
      | EVecFromToStep Expr Expr Expr
      | ECall T.Text [Expr]
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
      | ENegate Expr
      | ENumber Double
      | EStr T.Text
      | EColon
    deriving (Show, Eq)

expr :: Parser Expr
expr = buildExpressionParser expr_table noop_expr

expr_table :: OperatorTable [Token] () Identity Expr
expr_table
  = [
    [bin TPow EPow AssocRight],
    [Prefix $ token TSub >> return ENegate],
    [binl TMul EMul, binl TDiv EDiv],
    [binl TAdd EAdd, binl TSub ESub],
    [Infix
      (token TColon
        >> return
          (\left right
            -> case right of
              EVecFromTo e1 e2 -> EVecFromToStep left e1 e2
              _ -> EVecFromTo left right))
      AssocRight],
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
  = paren_expr
    <|> literal_expr
    <|> vec_expr
    <|> call_expr EVar ECall
    <|> (token TColon >> return EColon)

literal_expr :: Parser Expr
literal_expr
  = literal_num
    <|> literal_str

literal_num :: Parser Expr
literal_num = ENumber <$> number

literal_str :: Parser Expr
literal_str
  = do
    (TStr s) <- token $ TStr ""
    return $ EStr s

vec_expr :: Parser Expr
vec_expr
  = token TOSB >> EVec <$> sepBy expr (optional $ token TComma) <* token TCSB

call_expr :: (T.Text -> a) -> (T.Text -> [Expr] -> a) -> Parser a
call_expr cvar ccall = try (liftM2 ccall iden parameters) <|> cvar <$> iden

parameters :: Parser [Expr]
parameters = token TOP >> sepBy expr (token TComma) <*token TCP

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
