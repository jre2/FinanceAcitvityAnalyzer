{-# LANGUAGE OverloadedStrings #-}
module Data.Chase.Activity where

import qualified Data.ByteString.Char8 as B
import Data.Either
import Data.List
import Data.Maybe
import Data.Time
import Finance.Activity
import qualified Text.CSV as CSV
import Text.Regex.PCRE.Light
import System.Locale

-- Parsing CSV
loadCsv path = CSV.parseCSVFromFile path >>= return . filter (/= [""]) . head . rights . (: [])
loadTrans path = loadCsv path >>= return . csvToTrans . map (map B.pack)

csvToTrans = map rowToTrans
rowToTrans (a:b:c:d:[]) = Tran a (mkDate $ parseDate b c) (mkDate b) (extractMsg c) (read $ B.unpack d)

-- Extract more accurate date
parseDate dateField msgField = dateStr
    where dateStr = case match (compile "\\d\\d/\\d\\d" []) msgField [] of
                    Nothing -> dateField -- failback to proccessed date
                    Just (m:ms) -> B.concat [m, (B.drop 5 dateField)]

mkDate = fromJust . parseTime defaultTimeLocale "%m/%d/%Y" . B.unpack

-- Extract cleaner version of message
remReps r xs = foldr f [] xs
    where f x [] = x:[]
          f x acc = if x == r && (head acc) == r then acc else x:acc
stripSuffix pre s = maybe s reverse $ stripPrefix pre (reverse s)
extractMsg msg = clean $ maybe msg (!! 1) $ match (compile "U R\\* (.*)\\d\\d/\\d\\d" []) msg []
    where   clean = B.pack . stripSuffix " " . remReps ' ' . B.unpack
