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
-- * lines begins with '[X]' or '<X>' are completed tasks.
-- * lines begins with '[-]' or '<->' are completed tasks.
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
--  When you processed your TODO list for a day (or week, or month)
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
import Data.Maybe

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
-- FIXME line with white space only should be indent == 0
countIndent :: String -> Int
countIndent []   = 99999
countIndent (c:cs)
    | c  == ' '  = 1 + countIndent cs
    | c  == '\t' = tabwidth + countIndent cs
    | otherwise  = 0

isIndented (c:cs) = elem c " \t"
unIndent []     = []
unIndent (c:cs) | isIndented (c:cs) = unIndent cs
                | otherwise         = (c:cs)

minIndent :: Int -> String -> Bool
minIndent 0      line = True
minIndent indent line
    | indent <= countIndent line = True
    | otherwise                  = False


-------------------------------------------------------------------------------
data ItemQuery = Repeat | OneShot | RepeatDone | OneShotDone | Cancelled | AnyItem | Body | ToRemove
 | ToKeep | ToUndo
                 deriving (Eq, Show)
isItem  ::  ItemQuery -> String -> Bool
isItem q line@(c1:c2:c3:cs)
    | isIndented line    = isItem q (unIndent line)
    | q == OneShot       = (c1=='[' && c3==']')
    | q == Repeat        = (c1=='<' && c3=='>')
    | q == OneShotDone   = isPrefixOf "[X]" line || isPrefixOf "[x]" line
    | q == Cancelled     = isPrefixOf "[-]" line -- || isPrefixOf "<->" line
    | q == RepeatDone    = isPrefixOf "<X>" line || isPrefixOf "<x>" line
    | q == AnyItem       = isItem Repeat line || isItem OneShot line
    | q == Body          = not (isItem AnyItem line)
    | q == ToRemove      = isItem OneShotDone line || isItem Cancelled line
    | q == ToKeep        = isItem AnyItem line && not (isItem ToRemove line)
    | q == ToUndo        = isItem RepeatDone line || isItem Cancelled line
isItem _ _ = False


unDo_ ::  String -> String
unDo_ line@(c1:c2:cs)
   | isIndented line  = c1 : unDo_ (c2:cs)
   | otherwise        = (c1:' ':cs)


unDo :: String -> String
unDo line
    | isItem ToUndo line = unDo_ line
    | otherwise          = line


moreIndentThan line = minIndent (1 + countIndent line)


gtdSubItemParse :: Bool -> [String] -> Maybe [String]
gtdSubItemParse _ [] = Nothing
gtdSubItemParse isdone (l:ls)
    | isItem ToRemove l = case gtdSubItemTraverse True ls of
            Nothing  -> Nothing
            Just sub -> Just (l:sub)
    | isItem ToKeep l   = case gtdSubItemTraverse False ls of
            Nothing  -> Just [unDo l]
            Just sub -> Just ((unDo l):sub)
    | otherwise         = case gtdSubItemTraverse isdone ls of
            Nothing  -> if isdone then Nothing else Just [l]
            Just sub -> if isdone then Just sub else Just (l:sub)


gtdSubItemTraverse :: Bool -> [String] -> Maybe [String]
gtdSubItemTraverse _ [] = Nothing
gtdSubItemTraverse isdone (l:ls) =
    catMaybesOrNothing [head, tail]
    where head = gtdSubItemParse    isdone $ l:takeWhile (moreIndentThan l) ls
          tail = gtdSubItemTraverse isdone $   dropWhile (moreIndentThan l) ls

-- FIXME: more sophisticated way?
catMaybesOrNothing :: [Maybe [String]] -> Maybe [String]
catMaybesOrNothing maybelist = case cat of
    [ ]       -> Nothing
    otherwise -> Just cat
    where cat = concat $ catMaybes maybelist


gtdNextDay ls = case gtdSubItemTraverse False ls of
    Nothing -> []
    Just ls -> ls



-------------------------------------------------------------------------------
gtd_nextday ::  String -> String -> String
gtd_nextday _ []             = []
gtd_nextday datestr filedata = (updateDateLine l datestr)
                                ++  "\n"
                                ++ (unlines $ gtdNextDay ls)
                                where (l:ls) = lines filedata



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
