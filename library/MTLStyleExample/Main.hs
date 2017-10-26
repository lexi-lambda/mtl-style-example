module MTLStyleExample.Main
  ( main
  , mainIO
  ) where

import qualified Data.Text as T

import Prelude hiding (readFile)

import Control.Monad.Time (MonadTime(..))
import Control.Monad.Logger (LoggingT, MonadLogger(..), logInfoN, runStderrLoggingT)
import Data.Semigroup ((<>))
import Data.Time.Clock (diffUTCTime)

import MTLStyleExample.Interfaces

--------------------------------------------------------------------------------
-- IO wiring

newtype AppM a = AppM (LoggingT IO a)
  deriving ( Functor, Applicative, Monad
           , MonadArguments, MonadFileSystem, MonadLogger, MonadTime )

runAppM :: AppM a -> IO a
runAppM (AppM x) = runStderrLoggingT x

mainIO :: IO ()
mainIO = runAppM main

--------------------------------------------------------------------------------
-- Logic

main :: (MonadArguments m, MonadFileSystem m, MonadLogger m, MonadTime m) => m ()
main = do
  startTime <- currentTime
  [fileName] <- getArgs
  target <- readFile fileName
  logInfoN $ "Hello, " <> target <> "!"
  endTime <- currentTime
  let duration = endTime `diffUTCTime` startTime
  logInfoN $ T.pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"
