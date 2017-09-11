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

testdate1 = makeDate 02 07 2010
testdate2 = makeDate 10 07 2010
testdate3 = makeDate 19 07 2010
testdate4 = makeDate 02 08 2010

