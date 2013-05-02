--
-- gtdnextday.hs -- A GTD todo list recycler
--      2011-10-04 ryo1kato@gmail.com
--
-- This tiny Haskell program is to manage your text-based
-- TODO list.
--
-- The TODO list is just like a bullet list with some twist:
-- * lines begins with '[ ]' means tasks.
-- * lines begins with '< >' means reccuring tasks.
-- * lines begins with '[X]' or '<X>' is completed tasks.
-- * lines begins with '[-]' or '<->' is completed tasks.
--
--     | 2011-12-31  When an ISO format date is at begining of a file,
--     |             It indicates scope of this to do list.
--     | < > repatitive item
--     | [ ] one-shot item
--     |     [ ] can be indented
--     |         * this some non-todo items
--     |           and block quote
--     |           like this
--     | <X> completed repatitive item 
--     | [X] completed one-shot item done
--
--  When you processed your TODO list for a day(or week, or month)
--  and feed the file with this command, then this will generate a
--  converted output:
--
--  * A ISO-date notation at the beginning of the file (if any)
--    is updated to the current date.
--  * All non-completed TODOs are stay as it is.
--  * All completed, reccuring TODOs are cleared.
--    (revert to 'undone' status; the beginning of line is "< >")
--  * All completed or canceled TODOs are removed, along with it's memo.
--    (lines following, until next TODO item.
--    So far indents are not considered)
--

import System.Environment (getArgs)
import System.IO
import System.Time
import System.Locale
import Data.List
import Text.Regex

-------------------------------------------------------------------------------
tabwidth = 4 ::Int
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
countIndent :: String -> Int
countIndent []   = 0
countIndent (c:cs)
    | c  == ' '  = 1 + countIndent cs
    | c  == '\t' = tabwidth + countIndent cs
    | otherwise  = 0

isIndented (c:cs) = elem c " \t"
unIndent []     = []
unIndent (c:cs) | isIndented (c:cs) = unIndent cs
                | otherwise         = (c:cs)

data ItemQuery = Repeat | OneShot | RepeatDone | OneShotDone | OneShotCancel | Any
                 deriving (Eq, Show)
isItem  ::  ItemQuery -> String -> Bool
isItem q line@(c1:c2:c3:cs)
    | isIndented line    = isItem q (unIndent line)
    | q == OneShot       = (c1=='[' && c3==']')
    | q == Repeat        = (c1=='<' && c3=='>')
    | q == OneShotDone   = isPrefixOf "[X]" line || isPrefixOf "[x]" line
    | q == OneShotCancel = isPrefixOf "[-]" line || isPrefixOf "<->" line
    | q == RepeatDone    = isPrefixOf "<X>" line || isPrefixOf "<x>" line
    | q == Any           = isItem Repeat line || isItem OneShot line
    | otherwise          = False
isItem _ _ = False

isItemBody :: Int -> String -> Bool
isItemBody indent [] = True
isItemBody indent line@(c:cs)
    | indent == 0  = not (isItem Any line)
    | c == ' '     = isItemBody (indent-1) cs
    | c == '\t'    = isItemBody (indent-tabwidth) cs
    | otherwise    = False

dropItemBody :: String -> [String] -> [String]
dropItemBody l ls = dropWhile (isItemBody (countIndent l + 1)) ls

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
gtd_nextday_by_line [] = []
gtd_nextday_by_line (l:ls)
    | isItem OneShotDone l || isItem OneShotCancel l
                        = gtd_nextday_by_line (dropItemBody l ls)
    | isItem Repeat l   = unDone l : (takeWhile (isItemBody 0) ls
                                 ++ gtd_nextday_by_line (dropWhile (isItemBody 0) ls) )
    | otherwise         = l : (takeWhile (isItemBody 0) ls
                                 ++ gtd_nextday_by_line (dropWhile (isItemBody 0) ls) )


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
