import System.Random
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Lojban (isVowel)
import Lujvo (bestLujvo)
import Rafsi (rafsiMap)

parts  = [k | (k,v) <- M.toList rafsiMap]
finals = [k | (k,v) <- M.toList rafsiMap, any (isVowel . last) v]

randomChoiceIO :: (MonadIO m) => [a] -> m a
randomChoiceIO xs = fmap (xs!!) $ randomRIO (0, length xs - 1)

main :: IO ()
main = forever $ do
  x <- randomChoiceIO parts
  y <- randomChoiceIO finals
  putStrLn (bestLujvo [x,y])
