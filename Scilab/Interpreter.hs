module Scilab.Interpreter (interpret, Value (..), updateVector) where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (void, (>=>), liftM2)
import Control.Arrow (first, second)
import Data.List (nub)
import Data.Monoid ((<>))

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
    ix_ <- evalScalar ix
    rvi var 1 ix_ expr
exec (CAttr (RVI var [ixl, ixc]) expr)
  = do
    ixl_ <- evalScalar ixl
    ixc_ <- evalScalar ixc
    rvi var ixl_ ixc_ expr
exec (CAttr (RVI {}) _) = error "exec (CAttr (RVI {}) _)"
exec (CExpr expr) = void $ eval expr
exec c@(CWhile expr body) = ifS expr (body ++ [c]) []
exec (CFor var expr body) = evalVec expr >>= V.mapM_ (forLoop var body)

rvi :: T.Text -> Double -> Double -> Expr -> Scilab ()
rvi var ixl ixc expr
  = do
    vars <- getVars
    expr_@(Number typeNew _ _) <- eval expr
    let
      old
        = M.findWithDefault (Number typeNew V.empty 0) var vars
    modify
      $ first
      $ const
      $ M.insert var (updateVector ixl ixc expr_ old) vars

updateVector :: Double -> Double -> Value -> Value -> Value
updateVector ixl ixc (Number typeNew new 1) (Number typeOld old oldLines)
  = Number (typeOld && typeNew) newv newLines
    where
      ixl_ = fromEnum ixl
      ixc_ = fromEnum ixc
      oldSize = V.length old
      oldColumns = oldSize `div` oldLines
      newLines = oldLines `max` ixl_
      newColumns = oldColumns `max` ixc_
      ix = pred ixc_ * newLines + pred ixl_
      fill = V.replicate (newLines - oldLines) 0
      withNewLines
        = intercalateEnd
            fill
            (map
              (\x -> V.slice x oldLines old)
              [0, oldLines .. oldSize - oldLines])
      withNewColumns
        = withNewLines V.++ V.replicate ((newColumns - oldColumns) * newLines) 0
      newv = withNewColumns V.// [(ix, V.head new)]
updateVector _ _ _ _ = error "updateVector _"

intercalateEnd :: V.Vector a -> [V.Vector a] -> V.Vector a
intercalateEnd _ [] = V.empty
intercalateEnd y (x : xs) = x V.++ y V.++ intercalateEnd y xs

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
    let newv = V.concat $ map valueVec values
    return
      $ Number (and $ map valueBool values) newv 1

eval (EAdd e1 e2)
  = do
    v1 <- eval e1
    v2 <- eval e2
    return
      $ case v1 of
        Number _ vec1 _
          -> case v2 of
            Number _ vec2 _ -> opV (+) vec1 vec2
            _ -> error "eval EAdd"
        String vec1
          -> case v2 of
            String vec2 -> String $ V.zipWith (<>) vec1 vec2
            _ -> error "eval EAdd"
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
eval (ECall "sum" [e]) = vf V.sum e
eval (ECall "printf" (_ : es)) = mapM_ disp es >> return undefined
eval (ECall "max" [e]) = vf V.maximum e
eval (ECall "min" [e]) = vf V.minimum e
eval (ECall "length" [e]) = vf (toEnum . V.length) e
eval (ECall "mean" [e]) = vf (\x -> V.sum x / toEnum (V.length x)) e
eval (ECall "modulo" [e1, e2])
  = opD (\v1 v2 -> v1 - toEnum (fromEnum (v1 / v2)) * v2) e1 e2
eval (ECall "unique" [e])
  = vec
    <$> V.fromList
    <$> nub
    <$> V.toList
    <$> (evalVec e :: Scilab (V.Vector Double))
eval (ECall "sci2exp" [e])
  = do
    value <- eval e
    return
      $ String
      $ V.singleton
      $ case value of
        Number typ v _
          | V.length v == 1 -> showType typ $ V.head v
          | otherwise
            -> "["
              <> T.intercalate "," (V.toList $ V.map (showType typ) v)
              <> "]"
        String v
          | V.length v == 1 -> V.head v
          | otherwise -> T.pack $ show $ V.toList v
eval (ECall "strcat" [e])
  = String <$> V.singleton <$> T.concat <$> V.toList <$> getStrVec <$> eval e
eval (ECall var [ix])
  = do
    (Number typeVec v _) <- readVar var
    (Number typeIx ix_ _) <- eval ix
    let
      newv
        = case typeIx of
          False -> V.map ((v V.!) . pred . fromDouble) $ ix_
          True -> V.map fst $ V.filter snd $ V.zip v $ V.map fromDouble ix_
    return $ Number typeVec newv 1
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

showType :: Bool -> Double -> T.Text
showType True 1 = "%t"
showType True 0 = "%f"
showType False d = T.pack $ show d
showType _ _ = error "showType _ _"

disp :: Expr -> Scilab ()
disp = evalVec >=> tell . V.toList

evalVec :: Valuable a => Expr -> Scilab (V.Vector a)
evalVec e = getVec <$> eval e

evalScalar :: Valuable a => Expr -> Scilab a
evalScalar e = getScalar <$> eval e

evalScalarD :: Expr -> Scilab Double
evalScalarD = evalScalar

op :: (Valuable a, Valuable b) => (a -> a -> b) -> Expr -> Expr -> Scilab Value
op f e1 e2 = liftM2 (opV f) (evalVec e1) (evalVec e2)

opV
  :: (Valuable a, Valuable b)
    => (a -> a -> b) -> V.Vector a -> V.Vector a -> Value
opV f v1 v2
  = vec
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

vf :: (V.Vector Double -> Double) -> Expr -> Scilab Value
vf f e = scalar <$> f <$> evalVec e

type Scilab = StateT (M.Map T.Text Value, [Double]) (Writer [Double])

readVar :: T.Text -> Scilab Value
readVar var = (M.! var) <$> getVars

getVars :: Scilab (M.Map T.Text Value)
getVars = gets fst

data Value
  = Number {valueBool :: Bool, valueVec :: V.Vector Double, _valueLines :: Int}
      | String {_valueStrVec :: V.Vector T.Text}
    deriving (Show, Eq)

instance NFData Value where
  rnf (Number b v s) = b `seq` v `seq` s `seq` ()
  rnf (String v) = v `seq` ()

class Enum a => Valuable a where
  vec :: V.Vector a -> Value
  vec v = Number (not $ isDouble $ V.head v) (V.map toDouble v) (V.length v)

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

getStrVec :: Value -> V.Vector T.Text
getStrVec (String v) = v
getStrVec _ = error "getStrVec _"
