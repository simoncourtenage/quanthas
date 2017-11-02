module Tests.CalendarTest (calendarTestGroup) where

import QuantHas.Time.Date
import QuantHas.Time.TimeUnit
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Calendar.UnitedKingdom
import qualified QuantHas.Time.Calendar.Functions as CF
import Test.Tasty (testGroup)
import Test.Tasty.HUnit


calendarTestGroup = testGroup "Calendar Tests"
    [
         calendar_test1
       , calendar_test2
       , calendar_advance_test1
       , calendar_advance_test2
       , calendar_advance_test3
       , calendar_advance_test4
       , calendar_advance_test5
    ]

calendar_test1 = testCase "UK Calendar isBusinessDay" $ assertEqual "is business day" expected actual
    where actual   = isBusinessDay calendarUKLSE testdate1
          expected = True
 
calendar_test2 = testCase "UK Calendar isBusinessDay" $ assertEqual "is not business day" expected actual
    where actual   = isBusinessDay calendarUKLSE testdate2
          expected = False   

calendar_advance_test1 = testCase "UK Calendar advance 5 days" $ assertEqual "" expected actual
  where actual = CF.advanceDateByUnit calendarUKLSE testdate5 5 Days Following True
        expected = testdate6

calendar_advance_test2 = testCase "UK Calendar advance 5 weeks" $ assertEqual "" expected actual
  where actual = CF.advanceDateByUnit calendarUKLSE testdate7 5 Weeks Following True
        expected = testdate8

calendar_advance_test3 = testCase "UK Calendar advance 5 months" $ assertEqual "" expected actual
  where actual = CF.advanceDateByUnit calendarUKLSE testdate9 5 Months Following True
        expected = testdate10

-- advancing three months from 31st January gives 30th April, which is a Sunday.  The expected result
-- therefore is 28th April, moving back two days to the last business day (Friday).
calendar_advance_test4 = testCase "UK Calendar advance 3 months (end of month)" $ assertEqual "" expected actual
  where actual = CF.advanceDateByUnit calendarUKLSE testdate11 3 Months Following True
        expected = testdate12

-- advance by months but not end of month
calendar_advance_test5 = testCase "UK Calendar advance 9 months (not end of month)" $ assertEqual "" expected actual
  where actual = CF.advanceDateByUnit calendarUKLSE testdate13 9 Months Following True
        expected = testdate14

-- test data

testdate1 = gettestdate $ mkCalDate 07 02 2010
testdate2 = gettestdate $ mkCalDate 07 10 2010
testdate3 = gettestdate $ mkCalDate 04 25 2011 -- Easter Monday 2011
testdate4 = gettestdate $ mkCalDate 04 06 2011 -- Easter Friday 2012
testdate5 = gettestdate $ mkCalDate 10 05 2017
testdate6 = gettestdate $ mkCalDate 10 12 2017
testdate7 = gettestdate $ mkCalDate 08 30 2017
testdate8 = gettestdate $ mkCalDate 10 04 2017
testdate9 = gettestdate $ mkCalDate 02 06 2017
testdate10 = gettestdate $ mkCalDate 07 06 2017
testdate11 = gettestdate $ mkCalDate 01 31 2017
testdate12 = gettestdate $ mkCalDate 04 28 2017
testdate13 = gettestdate $ mkCalDate 01 01 2017
testdate14 = gettestdate $ mkCalDate 10 02 2017

gettestdate (Just d) = d