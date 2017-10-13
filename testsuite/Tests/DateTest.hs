module Tests.DateTest (dateTestGroup) where

import QuantHas.Time.Date
import QuantHas.Time.Period 
import QuantHas.Time.TimeUnit
import Test.Tasty (testGroup)
import Test.Tasty.HUnit
import Data.Maybe (fromJust)

dateTestGroup
    = testGroup "Date Tests"
        [
            date_test_advance1,
            date_test_advance2,
            date_test_advance3,
            endofmonth_test1,
            endofmonth_test2,
            endofmonth_test3,
            subtract_dates_test1,
            subtractFromDate_test1
        ]

date_test_advance1 = testCase "Date advance test - days" $ assertEqual "" expected actual
    where actual   = advance testdate1 15 Days
          expected = testdate2

date_test_advance2 = testCase "Date advance test - weeks" $ assertEqual "" expected actual
    where actual   = advance testdate1 3 Weeks
          expected = testdate3

date_test_advance3 = testCase "Date advance test - months" $ assertEqual "" expected actual
    where actual   = advance testdate1 3 Months
          expected = testdate4

endofmonth_test1 = testCase "End of month - July 2017" $ assertEqual "" expected actual
    where actual = endOfMonth testdate1
          expected = endjuly17

endofmonth_test2 = testCase "End of month - Feb 2017" $ assertEqual "" expected actual
    where actual = endOfMonth testdate5
          expected = endfeb17

endofmonth_test3 = testCase "End of month - Feb 2012" $ assertEqual "" expected actual
    where actual = endOfMonth testdate6
          expected = endfeb12

subtract_dates_test1 = testCase "Subtract dates" $ assertEqual "" expected actual
    where actual = subtractDates endfeb12 nov_13_11
          expected = (30-13+31+31+29)

subtractFromDate_test1 = testCase "Subtract from date" $ assertEqual "" expected actual
    where actual = subtractFromDate endfeb12 (mkPeriodFromTime 1 Months)
          expected = jan_29_12


-- test dates 

testdate1 = fromJust $ mkCalDate 07 25 2017
testdate2 = fromJust $ mkCalDate 08 09 2017
testdate3 = fromJust $ mkCalDate 08 15 2017
testdate4 = fromJust $ mkCalDate 10 25 2017
testdate5 = fromJust $ mkCalDate 02 13 2017
testdate6 = fromJust $ mkCalDate 02 11 2012

nov_13_11 = fromJust $ mkCalDate 11 13 2011
endjuly17 = fromJust $ mkCalDate 07 31 2017
endfeb17  = fromJust $ mkCalDate 02 28 2017
endfeb12  = fromJust $ mkCalDate 02 29 2012
jan_29_12 = fromJust $ mkCalDate 01 29 2012