module Scilab.Interpreter (interpret) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (void, (>=>))
import Control.Arrow (first, second)

-- deepseq
import Control.DeepSeq (NFData (rnf))

-- containers
import qualified Data.Map as M

-- vector
import qualified Data.Vector as V

-- text
import qualified Data.Text as T

-- transformers
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.Monad.Trans.State (StateT, execStateT)

-- mtl
import Control.Monad.Writer.Class (tell)
import Control.Monad.State.Class (gets, modify)

-- scilab
import Scilab.Parser

interpret :: [Double] -> T.Text -> ([Double], [Double])
interpret input = run input . parser

run :: [Double] -> [Command] -> ([Double], [Double])
run input cs = runWriter $ snd <$> execStateT (execs cs) (M.empty, input)

execs :: [Command] -> Scilab ()
execs = mapM_ exec

exec :: Command -> Scilab ()
exec (CIf expr then_ else_) = ifS expr then_ else_
exec (CAttr (RVar var) e) = eval e >>= attr var
exec (CAttr (RVI var [ix]) expr)
  = do
    ix_ <- pred <$> evalScalar ix
    vars <- getVars
    let (Number typeOld old) = vars M.! var
    (Number typeNew new) <- eval expr
    modify
      $ first
      $ const
      $ M.insert
        var
        (Number (typeOld && typeNew) $ old V.// [(ix_, V.head new)])
        vars
exec (CAttr (RVI {}) _) = error "exec (CAttr (RVI {}) _)"
exec (CExpr expr) = void $ eval expr
exec c@(CWhile expr body) = ifS expr (body ++ [c]) []
exec (CFor var expr body) = evalVec expr >>= V.mapM_ (forLoop var body)

ifS :: Expr -> [Command] -> [Command] -> Scilab ()
ifS cond then_ else_
  = do
    result <- evalScalar cond
    if result then execs then_ else execs else_

attr :: T.Text -> Value -> Scilab ()
attr var = modify . first . M.insert var

forLoop :: T.Text -> [Command] -> Double -> Scilab ()
forLoop var body cur = attr var (scalar cur) >> execs body

eval :: Expr -> Scilab Value
eval (EVar var) = readVar var
eval (EVec exprs)
  = do
    values <- mapM eval exprs
    return
      $ Number (and $ map valueBool values)
      $ V.concat $ map valueVec values
eval (EAdd e1 e2) = opD (+) e1 e2
eval (ESub e1 e2) = opD (-) e1 e2
eval (EMul e1 e2) = opD (*) e1 e2
eval (EDiv e1 e2) = opD (/) e1 e2
eval (EPow e1 e2) = opD (**) e1 e2
eval (EEq e1 e2) = opD (==) e1 e2
eval (EDiff e1 e2) = opD (/=) e1 e2
eval (EGT e1 e2) = opD (>) e1 e2
eval (EGTE e1 e2) = opD (>=) e1 e2
eval (ELT e1 e2) = opD (<) e1 e2
eval (ELTE e1 e2) = opD (<=) e1 e2
eval (EAnd e1 e2) = op (&&) e1 e2
eval (EOr e1 e2) = op (||) e1 e2
eval (ENot e) = dof not e
eval (ENegate e) = dofD negate e
eval (ENumber n) = return $ scalar n
eval (EStr t) = return $ String $ V.singleton t
eval (ECall "input" _) = scalar <$> head <$> gets snd <* modify (second tail)
eval (ECall "disp" [e]) = disp e >> return undefined
eval (ECall "sqrt" [e]) = dofD sqrt e
eval (ECall "factorial" [e]) = dofD (product . enumFromTo 1) e
eval (ECall "sum" [e])
  = scalar <$> (V.sum :: V.Vector Double -> Double) <$> evalVec e
eval (ECall "printf" (_ : es)) = mapM_ disp es >> return undefined
eval (ECall var [ix])
  = do
    (Number typeVec v) <- readVar var
    (Number typeIx ix_) <- eval ix
    return
      $ Number typeVec
      $ case typeIx of
        False -> V.map ((v V.!) . pred . fromDouble) $ ix_
        True -> V.map fst $ V.filter snd $ V.zip v $ V.map fromDouble ix_
eval (ECall {}) = error "eval (ECall {})"
eval (EVecFromTo from to)
  = do
    nfrom <- evalScalarD from
    nto <- evalScalarD to
    return $ vec $ V.fromList [nfrom .. nto]
eval (EVecFromToStep from step to)
  = do
    nfrom <- evalScalarD from
    nstep <- evalScalarD step
    nto <- evalScalarD to
    return $ vec $ V.fromList [nfrom, (nfrom + nstep) .. nto]

disp :: Expr -> Scilab ()
disp = evalVec >=> tell . V.toList

evalVec :: Valuable a => Expr -> Scilab (V.Vector a)
evalVec e = getVec <$> eval e

evalScalar :: Valuable a => Expr -> Scilab a
evalScalar e = getScalar <$> eval e

evalScalarD :: Expr -> Scilab Double
evalScalarD = evalScalar

op :: (Valuable a, Valuable b) => (a -> a -> b) -> Expr -> Expr -> Scilab Value
op f e1 e2
  = do
    v1 <- evalVec e1
    v2 <- evalVec e2
    return
      $ vec
      $ (if V.length v1 == 1
          then V.map . f . V.head
          else if V.length v2 == 1
            then flip (V.map . flip f . V.head)
            else V.zipWith f)
        v1
        v2

opD :: Valuable a => (Double -> Double -> a) -> Expr -> Expr -> Scilab Value
opD = op

dof :: (Valuable a, Valuable b) => (a -> b) -> Expr -> Scilab Value
dof f e = vec <$> V.map f <$> evalVec e

dofD :: (Double -> Double) -> Expr -> Scilab Value
dofD = dof

type Scilab = StateT (M.Map T.Text Value, [Double]) (Writer [Double])

readVar :: T.Text -> Scilab Value
readVar var = (M.! var) <$> getVars

getVars :: Scilab (M.Map T.Text Value)
getVars = gets fst

data Value
  = Number {valueBool :: Bool, valueVec :: V.Vector Double}
      | String {_valueStrVec :: V.Vector T.Text}
    deriving (Show, Eq)

instance NFData Value where
  rnf (Number b v) = b `seq` v `seq` ()
  rnf (String v) = v `seq` ()

class Enum a => Valuable a where
  vec :: V.Vector a -> Value
  vec v = Number (not $ isDouble $ V.head v) $ V.map toDouble v

  getVec :: Value -> V.Vector a
  getVec = V.map fromDouble . valueVec

  getScalar :: Value -> a
  getScalar = V.head . getVec

  scalar :: a -> Value
  scalar = vec . V.singleton

  toDouble :: a -> Double
  toDouble = toEnum . fromEnum

  fromDouble :: Double -> a
  fromDouble = toEnum . fromEnum

  isDouble :: a -> Bool
  isDouble _ = True

instance Valuable Double where
  toDouble = id
  fromDouble = id

instance Valuable Bool where
  fromDouble = (/= 0)
  isDouble _ = False

instance Valuable Int
