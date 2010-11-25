{-# LANGUAGE OverloadedStrings #-}
module Finance.Activity where

import qualified Data.ByteString.Char8 as B
import Data.Ord
import Data.Tag
import Data.Time
import qualified Text.CSV as CSV

-- Types
type TType = Str
type Date = Day
type Msg = Str
type Amt = Double
data Tran = Tran { ttype::TType, date::Date, proccessed::Date, msg::Msg, amt::Amt } deriving (Show, Read, Eq)

instance Ord Tran where
    compare a b = comparing date a b

-- Date Filtering
isBetween (y1,m1,d1) (y2,m2,d2) t = (fromGregorian y1 m1 d1) <= d && d <= (fromGregorian y2 m2 d2)
    where d = date $ fst t
between a b = filter (isBetween a b)

inMonth :: Integer -> Int -> Tagged Tran -> Tagged Tran
inMonth y m = filter (\tt -> let d = date $ fst tt in a <= d && d <= b)
    where a = fromGregorian y m 0
          b = addDays (-1) $ fromGregorian y (m+1) 0

fByMonth f y ts = map (\m -> f $ inMonth y m ts) [1..12]

-- Amt Filtering
getExpenses :: Tagged Tran -> Tagged Tran
getExpenses = filter (not . hasTag "income")

getIncomes :: Tagged Tran -> Tagged Tran
getIncomes = filter (hasTag "income")

amtIn = sum . map (amt . fst) . getIncomes
amtOut = sum . map (amt . fst) . getExpenses
amtDiff ts = amtIn ts + amtOut ts

amtInMonthly = fByMonth amtIn
amtOutMonthly = fByMonth amtOut
amtDiffMonthly = fByMonth amtDiff

spentOn t ts = amtOut $ withTag t ts
withAmt f = filter (f . amt . fst)
