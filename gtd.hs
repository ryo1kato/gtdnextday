--
-- GTD.hs
--      2010-05-07 ryo1.kato@gmail.com
--

import System.Environment (getArgs)
import IO
import Data.List

-------------------------------------------------------------------------------
isIndented (l:ls) = elem l " \t"
unIndent (l:ls) | isIndented (l:ls) = unIndent ls
                | otherwise         = (l:ls)

data ItemQuery = RepeatDone | OneShotDone | Any
                 deriving (Eq)
isItem  ::  ItemQuery -> String -> Bool
isItem q line@(c1:c2:c3:cs)
    | isIndented line   = isItem q (unIndent line)
    | q == OneShotDone  = isPrefixOf "[X]" line
    | q == RepeatDone   = isPrefixOf "<X>" line
    | q == Any          = (c1=='[' && c3==']') ||  (c1=='<' && c3=='>')
    | otherwise         = False
isItem _ _ = False



-------------------------------------------------------------------------------

gtd_nextday ::  String -> String
gtd_nextday = unlines . gtd_nextday_by_line . lines

unDone ::  String -> String
unDone line@(c1:c2:cs)
   | isIndented line  = unDone (c2:cs)
   | otherwise        = (c1:' ':cs)

gtd_nextday_by_line :: [String] -> [String]
gtd_nextday_by_line []    = []
gtd_nextday_by_line (l:ls)
    | isItem OneShotDone l = gtd_nextday_by_line (dropWhile (not . isItem Any) ls)
    | isItem RepeatDone  l = unDone l : (takeWhile (not . isItem Any) ls
                                 ++ gtd_nextday_by_line (dropWhile (not . isItem Any) ls) )
    | otherwise            = l : (takeWhile (not . isItem Any) ls
                                 ++ gtd_nextday_by_line (dropWhile (not . isItem Any) ls) )


-------------------------------------------------------------------------------
-- Basic file IO
--
--   Takes zero, one, or two arguments and (input,output) is set to
--   (stdin, stdout), (arg, stdout), (arg, arg) resp.
--
interactWith function inputStream outputStream = do
    input <- hGetContents inputStream
    hPutStr outputStream (function input)


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

