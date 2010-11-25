{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Chase.Activity as Chase
import Data.Tag
import Finance.Activity

import qualified Data.ByteString.Char8 as B
import Data.List
import Data.List.Split
import Text.Printf
import Text.Regex.PCRE.Light
import System.Environment
import System.IO.Unsafe

-- Parse rules file, ignoring empty lines and comments
parseRules = map (\(r,ts) -> (B.pack r, map B.pack ts) )
  . map (\[r,ts] -> (r, splitOn ", " ts) ) . map (splitOn " = ")
  . filter (not . isPrefixOf "--") . filter (/= "") . lines
getRules path = readFile path >>= return . parseRules

-- Main
rules' = getArgs >>= getRules . (!! 0)
rules = mkRules [caseless] $ unsafePerformIO rules'

trans' = unsafePerformIO $ getArgs >>= Chase.loadTrans . (!! 1)
trans = mkTaggedWrt [] rules trans' msg
main = do
   ppm $ withTag "food" trans
   print $ amtOutMonthly 2010 trans

-- Pretty Printing
pp :: (Show a) => [a] -> IO ()
pp = mapM_ (putStrLn . show)

ppm = pp . (map (\x -> printf "%8.2f %s" (amt x) (B.unpack $ msg x) ) :: [Tran]->[String]) . map fst

-- Debugging while writting rules
needRule = (pp $ map msg $ untagged trans) >> (print $ length $ untagged trans)
haveTag t = pp $ withTag t trans
msgs = map (msg . fst) trans
fooMsg = msg $ fst $ head $ trans
