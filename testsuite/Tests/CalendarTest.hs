module Tests.CalendarTest (calendarTestGroup) where

import QuantHas.Time.Date
import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Calendar.UnitedKingdom
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

{--
calendartests = TestList [TestLabel "Calendar test1" calendar_test1,
                          TestLabel "Calendar test2" calendar_test2,
                          TestLabel "Calendar test3" calendar_test3]
--}

calendarTestGroup = testGroup "Calendar Tests"
    [calendar_test1,calendar_test2]

calendar_test1 = testCase "UK Calendar isBusinessDay" $ assertEqual "is business day" expected actual
    where actual   = isBusinessDay calendarUKLSE testdate1
          expected = Right True
 
calendar_test2 = testCase "UK Calendar isBusinessDay" $ assertEqual "is not business day" expected actual
    where actual   = isBusinessDay calendarUKLSE testdate2
          expected = Right False

{--
calendar_test1 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay calendarUKLSE testdate1
          expected = True
 
calendar_test2 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay calendarUKLSE testdate2
          expected = False
--}

{--          
calendar_test3 = TestCase (assertEqual "UK Calendar isBusinessDay" expected actual)
    where actual   = isBusinessDay calendarUKLSE testdate3
          expected = False
--}       
          
          
-- test data

testdate1 = gettestdate $ mkDate 07 02 2010
testdate2 = gettestdate $ mkDate 07 10 2010
testdate3 = gettestdate $ mkDate 04 25 2011 -- Easter Monday 2011
testdate4 = gettestdate $ mkDate 04 06 2011 -- Easter Friday 2012

gettestdate (Just d) = d