module
  Scilab.Interpreter
  (interpret, Value (..), Atom (..), Vec (..), Valuable (..))
  where

-- base
import Control.Applicative ((<$>), (<*))
import Control.Monad (liftM2, void)
import Control.Arrow (first, second)

-- containers
import qualified Data.Map as M

-- vector
import qualified Data.Vector as V

-- text
import qualified Data.Text as T

-- transformers
import Control.Monad.Trans.Writer (Writer, execWriter)
import Control.Monad.Trans.State (StateT, evalStateT)

-- mtl
import Control.Monad.Writer.Class (tell)
import Control.Monad.State.Class (gets, modify)

-- scilab
import Scilab.Parser

interpret :: [Value] -> T.Text -> [Value]
interpret input = run input . parser

type Scilab = StateT (M.Map T.Text Value, [Value]) (Writer [Value])

run :: [Value] -> [Command] -> [Value]
run input cs = execWriter $ evalStateT (execs cs) (M.empty, input)

execs :: [Command] -> Scilab ()
execs = mapM_ exec

exec :: Command -> Scilab ()
exec (CIf expr then_ else_)
  = do
    cond <- fromAtomValue <$> eval expr
    if cond then execs then_ else execs else_
exec (CAttr (RVar var) e) = eval e >>= modify . first . M.insert var
exec (CAttr (RVI var ix) expr)
  = do
    ix_ <- (fromEnum :: Double -> Int) <$> fromAtomValue <$> eval ix
    vars <- gets fst
    let (Vec old) = vars M.! var
    new
      <- case old of
          VecNumber v_
            -> VecNumber
              <$> (v_ V.//)
              <$> (: [])
              <$> (,) ix_
              <$> fromAtomValue
              <$> eval expr
          VecBool v_
            -> VecBool
              <$> (v_ V.//)
              <$> (: [])
              <$> (,) ix_
              <$> fromAtomValue
              <$> eval expr
    modify $ first $ const $ M.insert var (Vec new) vars
exec (CExpr expr) = void $ eval expr

eval :: Expr -> Scilab Value
eval (EVar var) = (M.! var) <$> gets fst
eval (EVec exprs)
  = do
    exprs_ <- V.mapM eval (V.fromList exprs)
    return
      $ if V.all isBool exprs_
        then Vec $ VecBool $ V.map fromAtomValue exprs_
        else if V.all isNumber exprs_
          then Vec $ VecNumber $ V.map fromAtomValue exprs_
          else error "Inconsistent vector!"
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
eval (ENot e) = dof not <$> eval e
eval (ENumber n) = return $ toAtom n
eval (EStr t) = return $ Atom $ VStr t
eval (ECall "input" _) = head <$> gets snd <* modify (second tail)
eval (ECall "disp" e)
  = do
    e_ <- eval e
    tell [e_]
    return e_
eval (ECall var ix)
  = do
    (Vec vec) <- (M.! var) <$> gets fst
    ix_ <- eval ix
    return
      $ case vec of
      VecNumber v_ -> filterVec ix_ v_
      VecBool v_ -> filterVec ix_ v_

filterVec :: Valuable a => Value -> V.Vector a -> Value
filterVec ix_ v_
  = case ix_ of
    Vec (VecNumber ns) -> toVec $ V.map ((v_ V.!) . fromEnum) ns
    Vec (VecBool bs) -> toVec $ V.map fst $ V.filter snd $ V.zip v_ bs
    Atom n -> toAtom $ v_ V.! (fromEnum :: Double -> Int) (fromAtom n)

opD :: Valuable a => (Double -> Double -> a) -> Expr -> Expr -> Scilab Value
opD = op

op :: (Valuable a, Valuable b) => (a -> a -> b) -> Expr -> Expr -> Scilab Value
op f e1 e2 = liftM2 (doop f) (eval e1) (eval e2)

data Value = Vec Vec | Atom Atom deriving (Eq, Show)

data Atom = AtomNumber Double | AtomBool Bool | VStr T.Text deriving (Eq, Show)

data Vec
  = VecNumber (V.Vector Double) | VecBool (V.Vector Bool) deriving (Eq, Show)

class Valuable a where
  fromAtom :: Atom -> a
  toAtom :: a -> Value
  fromVec :: Vec -> V.Vector a
  toVec :: V.Vector a -> Value

  fromAtomValue :: Value -> a
  fromAtomValue (Atom a) = fromAtom a
  fromAtomValue _ = error "fromAtomValue _"

  fromVecValue :: Value -> V.Vector a
  fromVecValue (Vec a) = fromVec a
  fromVecValue _ = error "fromVecValue _"

instance Valuable Double where
  fromAtom (AtomNumber n) = n
  fromAtom (AtomBool b) = conv b
  fromAtom _ = error "fromAtom _ :: Double"

  toAtom = Atom . AtomNumber

  fromVec (VecNumber v) = v
  fromVec (VecBool v) = V.map conv v

  toVec = Vec . VecNumber

conv :: (Enum a, Enum b) => a -> b
conv = toEnum . fromEnum

instance Valuable Bool where
  fromAtom (AtomBool b) = b
  fromAtom (AtomNumber b) = conv b
  fromAtom _ = error "fromAtom _ :: Bool"

  toAtom = Atom . AtomBool

  fromVec (VecBool v) = v
  fromVec (VecNumber v) = V.map conv v

  toVec = Vec . VecBool

doop :: (Valuable a, Valuable b) => (a -> a -> b) -> Value -> Value -> Value
doop f (Vec v1) (Vec v2) = toVec $ V.zipWith f (fromVec v1) (fromVec v2)
doop f (Vec v) (Atom a) = toVec $ V.map (`f` fromAtom a) (fromVec v)
doop f (Atom a) (Vec v) = toVec $ V.map (f $ fromAtom a) (fromVec v)
doop f (Atom a1) (Atom a2) = toAtom $ fromAtom a1 `f` fromAtom a2

dof :: (Valuable a, Valuable b) => (a -> b) -> Value -> Value
dof f (Vec v) = toVec $ V.map f $ fromVec v
dof f (Atom a) = toAtom $ f $ fromAtom a

isNumber :: Value -> Bool
isNumber (Atom (AtomNumber _)) = True
isNumber _ = False

isBool :: Value -> Bool
isBool (Atom (AtomBool _)) = True
isBool _ = False
