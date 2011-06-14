--
-- gtd.hs -- A GTD todo list recycler
--      2010-05-07 ryo1.kato@gmail.com
--
-- > < > repatitive item
-- > [ ] one-shot item
-- >     [ ] can be indented
-- > <X> repatitive item done
-- > [X] one-shot item done
-- >

import System.Environment (getArgs)
import IO
import Time
import Locale
import Data.List
import Text.Regex

-------------------------------------------------------------------------------
midnight = 26 ::Int -- Start of a day. 24 to regular clocktime, 26 to 2 a.m.
dateStrRegex = mkRegex "^20[0-9][0-9]-[01][0-9]-[0-9][0-9]"
isDateLine :: String -> Bool
isDateLine line | match == Nothing  = False
                | otherwise         = True
                  where match = matchRegex dateStrRegex line

stripDate = drop 10

updateDateLine  ::  String -> String -> String
updateDateLine line datestr
    | isDateLine line   = datestr ++ stripDate line
    | otherwise         = line

myGetClockTime = do
    now        <- getClockTime
    return (addToClockTime timediff now)
    where timediff = TimeDiff 0 0 0 (24-midnight) 0 0 0

getDateStringToday = do
    nowoffset  <- myGetClockTime
    nowcaltime <- toCalendarTime(nowoffset)
    return (getDateString nowcaltime)

getDateString   ::  CalendarTime -> String
getDateString ct = formatCalendarTime defaultTimeLocale "%Y-%m-%d" ct


-------------------------------------------------------------------------------
isIndented (l:ls) = elem l " \t"
unIndent []     = []
unIndent (l:ls) | isIndented (l:ls) = unIndent ls
                | otherwise         = (l:ls)

data ItemQuery = Repeat | OneShot | RepeatDone | OneShotDone | OneShotCancel | Any
                 deriving (Eq, Show)
isItem  ::  ItemQuery -> String -> Bool
isItem q line@(c1:c2:c3:cs)
    | isIndented line    = isItem q (unIndent line)
    | q == OneShot       = (c1=='[' && c3==']')
    | q == Repeat        = (c1=='<' && c3=='>')
    | q == OneShotDone   = isPrefixOf "[X]" line
    | q == OneShotCancel = isPrefixOf "[-]" line
    | q == RepeatDone    = isPrefixOf "<X>" line
    | q == Any           = isItem Repeat line || isItem OneShot line
    | otherwise          = False
isItem _ _ = False

-- FIXME: Sofar line begins with non-space char are
--        item-body; lines, sections
--        We may want to check indent-level in the future.
isItemBody :: String -> Bool
isItemBody []          = False
isItemBody line@(c:cs) = not (isItem Any line) && c == ' '

unDone ::  String -> String
unDone line@(c1:c2:cs)
   | isIndented line  = c1 : unDone (c2:cs)
   | otherwise        = (c1:' ':cs)

-------------------------------------------------------------------------------
gtd_nextday ::  String -> String -> String
gtd_nextday _       []       = []
gtd_nextday datestr filedata = (updateDateLine l datestr) ++ "\n" ++ (unlines $ gtd_nextday_by_line ls)
                           where (l:ls) = lines filedata


gtd_nextday_by_line :: [String] -> [String]
gtd_nextday_by_line []    = []
gtd_nextday_by_line (l:ls)
    | isItem OneShotDone l || isItem OneShotCancel l
                        = gtd_nextday_by_line (dropWhile isItemBody ls)
    | isItem Repeat l   = unDone l : (takeWhile isItemBody ls
                                 ++ gtd_nextday_by_line (dropWhile isItemBody ls) )
    | otherwise         = l : (takeWhile isItemBody ls
                                 ++ gtd_nextday_by_line (dropWhile isItemBody ls) )


-------------------------------------------------------------------------------
-- Basic file IO
--
--   Takes zero, one, or two arguments and (input,output) is set to
--   (stdin, stdout), (arg, stdout), (arg, arg) resp.
--
interactWith function inputStream outputStream = do
    datestr <- getDateStringToday
    input <- hGetContents inputStream
    hPutStr outputStream (function datestr input)
    hFlush outputStream

main = mainWith myFunction
  where mainWith function = do
            args <- getArgs
            case args of
                [input,output] -> do inputStream  <- openFile input ReadMode
                                     outputStream <- openFile output WriteMode
                                     interactWith function inputStream outputStream
                [input]        -> do inputStream  <- openFile input ReadMode
                                     interactWith function inputStream stdout
                _              -> interactWith function stdin stdout
        -- replace "id" with the name of our function below
        --myFunction = id                    -- cat
        --myFunction = unlines.reverse.lines -- tac
        myFunction = gtd_nextday
