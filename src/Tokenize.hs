module Tokenize where
   
import           Text.Regex.PCRE (Regex, matchOnceText, makeRegex)
import qualified Text.Regex.PCRE as R
import           Data.Time (UTCTime, formatTime, defaultTimeLocale, parseTimeM)
import qualified Data.Time as T
import           Control.Monad (Monad, msum)
import qualified Control.Monad as M
import           Data.Maybe (Maybe, fromJust, isJust)
import qualified Data.Maybe as M
import           Data.Array (Array)
import qualified Data.Array as A
import           Debug.Trace (traceShow)
import qualified Debug.Trace as T

data Student = Student {
   studentNumber :: String,
   questID :: String
} deriving (Show, Eq, Ord)

data Token = TDate {
               tDate :: UTCTime
            }
           | TSectionChange {
               tStudent     :: Student,
               tFromSection :: String,
               tToSection   :: String
            }
         deriving (Show)

tokenize :: String -> [Token]
tokenize str = [ fromJust parsed | line <- lines str, let parsed = tokenizeLine line, isJust parsed ]

tokenizeLine :: String -> Maybe Token
tokenizeLine str = msum $ map ($ str) [tokenizeTDate, tokenizeTSectionChange]
           
tokenizeTDate :: String -> Maybe Token
tokenizeTDate str = TDate <$> parseTimeM True defaultTimeLocale "Date: %a, %-e %b %Y %X %z" str

tokenizeTSectionChange :: String -> Maybe Token
tokenizeTSectionChange str = do
   (_,captured,_) <- matchOnceText regex str
   let [_,sid,qid,from,to] = map fst $ A.elems captured
   Just $ TSectionChange (Student sid qid) from to
   where regex = makeRegex "(\\d+) : (\\w+): MEET_PRIMARY change from (\\d+) to (\\d+)" :: Regex
