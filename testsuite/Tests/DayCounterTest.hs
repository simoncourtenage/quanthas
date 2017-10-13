module Tests.DayCounterTest (dayCounterTestGroup) where

import QuantHas.Time.Date
import QuantHas.Time.DayCounter
import Test.Tasty (testGroup)
import Test.Tasty.HUnit
-- import Test.HUnit

dayCounterTestGroup
    = testGroup "Day CounterTests"
        [daycounter_test1,daycounter_test2]

daycounter_test1 = testCase "dayCount actual365fixed" $ assertEqual "" expected actual
    where actual   = dayCount actual365fixedDayCounter testdate1 testdate4
          expected = 31
          
daycounter_test2 = testCase "actual365fixed year fraction" $
                        assertBool "" (expected == actual)
    where actual = yearFraction actual365fixedDayCounter testdate1 testdate4 testdate2 testdate3
          expected = (31.0 / 365.0)

-- test data

testdate1 = gettestdate $ mkDate 07 02 2010
testdate2 = gettestdate $ mkDate 07 10 2010
testdate3 = gettestdate $ mkDate 07 19 2010
testdate4 = gettestdate $ mkDate 08 02 2010

gettestdate (Just d) = getCalDate d