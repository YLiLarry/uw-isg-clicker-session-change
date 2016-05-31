{-# LANGUAGE FlexibleContexts #-}

module Parse where

import           Tokenize (Token(..), Student)
import qualified Tokenize as T
import           Data.Time (UTCTime, formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.Time as T
import           Control.Monad.Except (MonadError(..))
import qualified Control.Monad.Except as E

data SectionChange = SectionChange {
   student     :: Student,
   fromSection :: String,
   toSection   :: String,
   date        :: UTCTime
} deriving (Show)

sanityCheck :: (MonadError String m) => [Token] -> m ()
sanityCheck [] 
   = throwError $ "No MEET_PRIMARY change found in the email input.\n"
sanityCheck ((TSectionChange _ _ _):_) 
   = throwError $ "Expected a 'Date' (in the header) before the email content,\n" ++
                  "but a 'MEET_PRIMARY change' was found first.\n"
sanityCheck _ = return ()

parse :: (MonadError String m) => [Token] -> m [SectionChange]
parse tokens = do
   sanityCheck tokens
   return $ parse' (head tokens) (tail tokens)

parse' :: Token -> [Token] -> [SectionChange]
parse' _ [] = []
parse' _ (newDate@(TDate _):rest) = parse' newDate rest
parse' processDate (newDate@(TSectionChange student from to):rest)
   = (SectionChange student from to $ tDate processDate) : parse' processDate rest

