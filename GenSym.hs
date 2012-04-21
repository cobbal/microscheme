module GenSym where
import Control.Monad.State

type GenSymState = State Integer

gensym :: String -> GenSymState String
gensym sym = do
  s <- get
  put (s + 1)
  return (sym ++ "$" ++ show s)

runGenSymState :: GenSymState a -> a
runGenSymState s = evalState s 0
