{-# LANGUAGE FlexibleContexts #-}

module Analyze where
   
import           Parse (SectionChange(..))
import qualified Parse as P
import           Tokenize (Student(..))
import qualified Tokenize as T
import           Data.Time (UTCTime(..), 
                            Day(..), 
                            DiffTime(..), 
                            secondsToDiffTime, 
                            formatTime, 
                            defaultTimeLocale)
import qualified Data.Time as T
import           Text.Printf (printf)
import qualified Text.Printf as P
import           Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Writer as W
import           Data.List ()
import qualified Data.List as L

import           Debug.Trace (traceShowId)
import qualified Debug.Trace as T



groupByStudent :: [SectionChange] -> [[SectionChange]]
groupByStudent = L.groupBy (\x y -> student x == student y) . L.sortOn student

sortByDate :: [SectionChange] -> [SectionChange]
sortByDate = L.sortOn date

sanityCheck :: (MonadWriter String m) => [SectionChange] -> m ()
sanityCheck [] = return ()
sanityCheck [a] = return ()
sanityCheck (a:b:rest)
   | (toSection a) == (fromSection b) = sanityCheck (b:rest)
   | otherwise = do 
         tell $ printf ("Student %s transferred from section %s to section %s, \n" ++
                        "but the next email says s/he transferred from %s, \n" ++
                        "indicating there might be an email missing in between.\n")
                  qid from to fromb
         sanityCheck (b:rest)
      where 
         qid   = questID $ student a
         from  = fromSection a
         to    = toSection   a
         fromb = fromSection b

toClickerOverwrite :: (MonadWriter String m) => [SectionChange] -> m String
toClickerOverwrite ls = L.unlines <$> mapM toOverwriteEntries (groupByStudent ls)

toOverwriteEntries :: (MonadWriter String m) => [SectionChange] -> m String
toOverwriteEntries ls = do
   let a = sortByDate ls
   sanityCheck a
   return $ toOverwriteEntries' a
   
toOverwriteEntries' :: [SectionChange] -> String
toOverwriteEntries' [] = ""
toOverwriteEntries' ls = L.unlines $ L.zipWith mkOverwriteEntry (init : map date ls) ls
   where
      init = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
      qid = questID $ student $ head ls 
      mkOverwriteEntry :: UTCTime -> SectionChange -> String
      mkOverwriteEntry d sc = 
         printf "sections:%s:%s:%s:%s:%s" 
            qid (showDate d) (fromSection sc) (showDate $ date sc) (toSection sc)
      showDate = formatTime defaultTimeLocale "%F"

