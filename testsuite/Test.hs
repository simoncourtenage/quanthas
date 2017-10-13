module Main where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual,testCase)
-- import Test.HUnit
import Tests.DayCounterTest (dayCounterTestGroup)
import Tests.CalendarTest (calendarTestGroup)
import Tests.DateTest (dateTestGroup)

-- runtests1 = runTestTT daycountertests
-- runtests2 = runTestTT calendartests

main = defaultMain unitTests

unitTests =
    testGroup " Unit tests"
    [calendarTestGroup,dayCounterTestGroup,dateTestGroup]

{--
main = do
    putStrLn "Running daycounter tests"
    runtests1
    putStrLn "Running calendar tests"
    runtests2
--}