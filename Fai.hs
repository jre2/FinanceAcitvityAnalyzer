{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Chase.Activity as Chase
import Finance.Activity
import Data.Tag

import qualified Data.ByteString.Char8 as B
import Text.Printf
import Text.Regex.PCRE.Light
import System.IO.Unsafe

ruleStrs =
    -- food
    [("chicken bar", ["food", "near home", "dinner", "chicken"])
    ,("subway", ["food", "dinner", "near home", "sandwich"])
    ,("papa johns", ["pizza", "dinner", "near home", "food"])
    ,("brother jimmy's", ["american", "dinner", "near home", "food"])
    ,("brothers famou", ["american", "dinner", "near home", "food"])
    ,("kodama restaurant", ["japanese", "lunch/dinner", "near home", "food"])
    ,("yoshinoya new york", ["japanese", "lunch/dinner", "near home", "food"])
    ,("chipotle", ["mexican", "food", "lunch/dinner", "near home"])
    ,("best burgers & sha", ["american", "food", "lunch", "near home"])
    ,("dunkin", ["american", "food", "breakfast", "near home"])
    ,("coldstone", ["ice cream", "food", "near home", "dinner", "desert"])
    ,("chef yu new york", ["chinese", "food", "dinner", "near home"])
    ,("nathans", ["food", "seafood", "dinner", "near home"])
    ,("church's chicken", ["food", "chicken", "dinner", "near home"])
    ,("royal thai cuisine", ["food", "thai", "dinner", "near home"])
    ,("hallo berlin", ["food", "german", "lunch", "near home"])
    ,("la paloma burritos", ["food", "mexican", "dinner", "near home"])
    ,("city market cafe", ["food", "lunch", "sandwich"])

    ,("baja fresh", ["mexican", "food", "lunch", "near work"])
    ,("oaxaca mexican", ["mexican", "food", "lunch", "near work"])
    ,("oriental noodle", ["chinese", "food", "lunch", "near work"])
    ,("china moon", ["chinese", "food", "lunch", "near work"])
    ,("joy curry", ["indian", "food", "lunch", "near work"])
    ,("chiyoda new york", ["japanese", "dinner", "near work", "food"])
    ,("RA@JP Morgan Chase", ["cafeteria", "near work", "food", "lunch"])

    ,("mcdonald's", ["american", "food", "lunch/dinner", "near home/work"])
    ,("withdrawal", ["withdrawal", "food", "lunch", "near work", "cash only"])
    ,("non-chase atm withdraw", ["atm","fee","withdrawal","food"])
    -- groceries
    ,("cafe zaiya", ["food", "groceries", "near work", "japanese"])
    ,("dainobu inc", ["food", "groceries", "near work", "japanese"])
    ,("duane reade", ["food", "groceries"])
    ,("walgreens", ["drugstore"])
    -- bills
    ,("online payment.*time warner", ["internet","bill"])
    ,("online payment.*sherrington holdings", ["rent","bill"])
    ,("online payment.*georgia re", ["loan","bill"])
    ,("student loan pmt payment", ["loan","bill"])
    ,("9th corner cleaner", ["laundry", "near home"])
    ,("niteen internation", ["hotel"])
    ,("MTA vending machines", ["travel", "subway"])
    -- fees
    ,("service fee", ["fee", "service fee"])
    -- entertainment
    ,("Museum", ["museum", "entertainment"])
    ,("MMA admissions", ["museum", "entertainment"])
    ,("amnh-satelite", ["museum", "entertainment"])
    ,("amc empire", ["cinema", "entertainment", "near home"])
    ,("kinokuniya", ["book", "near work", "entertainment"])
    -- online
    ,("www.newegg.com", ["online", "newegg", "electronics"])
    ,("paypal", ["paypal", "online", "misc goods"])
    ,("amazon.com", ["amazon", "online", "misc goods"])
    ,("blockbuster", ["blockbuster", "online", "games", "ps3"])
    ,("rhapsody", ["rhapsody", "online", "music"])
    -- income
    ,("jpmorgan chase b payroll", ["salary", "income"])
    ,("interest payment", ["income", "interest"])
    ,("deposit.*id.*number", ["income", "deposit"])
    -- test
    ,("never used", ["fake tag1", "fake tag2"])
    ]

-- Main
rules = mkRules [caseless] ruleStrs

trans' = unsafePerformIO $ Chase.loadTrans "JPMC.CSV"
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
