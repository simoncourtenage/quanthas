module Tests.ScheduleTest (scheduleTestGroup) where

import qualified QuantHas.Settings as Settings
import QuantHas.Time.Date
import QuantHas.Time.TimeUnit
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.DateGeneration
import QuantHas.Time.Period
import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Calendar.NullCalendar
import QuantHas.Time.Calendar.UnitedKingdom
import qualified QuantHas.Time.Calendar.Functions as CF
import QuantHas.Time.Schedule
import Data.Maybe (fromJust)
import Data.Either
import Test.Tasty (testGroup)
import Test.Tasty.HUnit


scheduleTestGroup = testGroup "Schedule Tests"
    [
           schedule_test1
         , schedule_test2
         , schedule_backwards_dates_1
         , schedule_backwards_dates_2 
    ]

schedule_test1 = testCase "Schedule datesList test" $ assertEqual "" expected actual
    where actual = last . take 7 $ datesList nullCalendar date1 1 Months Following False
          expected = date2

schedule_test2 = testCase "Schedule datesList test - negative" $ assertEqual "" expected actual
    where actual = last . take 7 $ datesList nullCalendar date2 (negate 1) Months Following False
          expected = date1

schedule_backwards_dates_1
    = testCase "Schedule Backwards Dates Test" $ assertEqual "" expected actual
        where actual = dates sched
              expected = scheddates
              sched    = fromRight $ scheduleDates begin end initSched
              begin    = fromJust $ mkDate 01 01 2017
              end      = fromJust $ mkDate 09 01 2017
              tenor    = Just $ mkPeriodFromTime 1 Months
              initSched = fromRight $
                            mkSchedule [] calendarUKLSE Following Nothing tenor (Just Backward) (Just False) []

schedule_backwards_dates_2
    = testCase "Schedule Backwards Dates Test 2" $ assertEqual "" expected actual
        where actual = dates sched
              expected = scheddates
              sched    = fromRight $
                            mkScheduleFromEffectiveDate
                                Settings.defaultSettings
                                begin
                                end
                                calendarUKLSE
                                Following
                                Nothing
                                tenor
                                (Just Backward)
                                (Just False)
                                NullDate
                                ntl
              begin    = fromJust $ mkDate 01 01 2017
              end      = fromJust $ mkDate 09 01 2017
              ntl      = fromJust $ mkDate 08 20 2017
              tenor    = Just $ mkPeriodFromTime 1 Months

date1 = fromJust $ mkCalDate 01 01 2017
date2 = fromJust $ mkCalDate 07 01 2017

scheddates
    =  fmap (fromJust . uncurry3 mkDate)
        [(1,1,2017),(2,1,2017),(3,1,2017),(4,1,2017),(5,1,2017),(6,1,2017),(7,1,2017),(8,1,2017),(9,1,2017)]

-- utils

fromRight :: Either String b -> b
fromRight (Right b) = b
fromRight (Left s)  = error $ "Left: " ++ s

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
