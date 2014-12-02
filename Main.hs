{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import System.IO
import qualified Data.Aeson as AE
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.ByteString.Char8 as AB8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.Binary as CB
import qualified EDN
import qualified Transform

data ParseProblem
  = ErroneousData B8.ByteString
  deriving Show

valueParser :: AB8.Parser EDN.Value
valueParser = EDN.value

valueTransformer :: EDN.Value -> BL.ByteString
valueTransformer = AE.encode . Transform.ednToJson

parsed :: A.Parser (Either e EDN.Value)
parsed = do
  value <- valueParser
  return $ Right value

unparsed :: A.Parser (Either ParseProblem a)
unparsed = do
  string <- A.scan () $ \_ ch -> if AB8.isEndOfLine ch then Nothing else Just ()
  return $ Left $ ErroneousData string

parser :: A.Parser (Either ParseProblem EDN.Value)
parser = (parsed <|> unparsed) <* many AB8.endOfLine

source :: (MonadIO m, MonadThrow m) => Source m (Either ParseProblem BL.ByteString)
source = mapOutput (fmap valueTransformer . snd) $ CB.sourceHandle stdin $= CA.conduitParser parser

sink :: (MonadResource m, Show e) => Sink (Either e BL.ByteString) m ()
sink = do
    a <- await
    case a of
      Nothing -> return ()
      Just result -> do
        liftIO $ case result of
          Left err -> hPrint stderr err
          Right out -> BL8.putStrLn out
        sink

main :: IO ()
main = runResourceT $ source $$ sink
