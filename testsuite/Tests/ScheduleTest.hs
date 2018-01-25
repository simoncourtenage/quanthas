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
         , schedule_backwards_regular
         , schedule_forwards_dates_1
         , schedule_forwards_regular
    ]

schedule_test1 = testCase "Schedule datesList test" $ assertEqual "" expected actual
    where actual   = take 7 $ datesList nullCalendar date1 1 Months Following False
          expected = datelist_test1

schedule_test2 = testCase "Schedule datesList test - negative" $ assertEqual "" expected actual
    where actual = last . take 7 $ datesList nullCalendar date2 (negate 1) Months Following False
          expected = date1

schedule_backwards_dates_1
    = testCase "Schedule Backwards Dates Test" $ assertEqual "" expected actual
        where actual = dates sched
              expected = scheddates
              sched    = fromRight $ genScheduleDates begin end initSched
              begin    = fromJust $ mkDate 01 01 2017
              end      = fromJust $ mkDate 09 01 2017
              tenor    = Just $ mkPeriodFromTime 1 Months
              initSched = fromRight $
                            mkSchedule [] calendarUKLSE Following Nothing tenor (Just Backward) (Just False) []

{--
    Quantlib, with these parameters, generates a schedule  with
    the following dates:
    January 3rd, 2017
    January 20th, 2017
    February 20th, 2017
    March 20th, 2017
    April 20th, 2017
    May 22nd, 2017
    June 20th, 2017
    July 20th, 2017
    August 21st, 2017
    September 1st, 2017
--}
schedule_backwards_dates_2
    = testCase "Schedule Backwards Dates Test 2" $ assertEqual "" expected actual
        where actual = dates sched
              expected = scheddates_adjusted
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

schedule_backwards_regular
    = testCase "Schedule Backwards Regular Test" $ assertEqual "" expected actual
    where actual = regular sched
          expected = regular_expected
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
          
schedule_forwards_dates_1
    = testCase "Schedule Forward Dates Test 1" $ assertEqual "" expected actual
    where actual = dates sched
          expected = scheddates_forward_adjusted
          sched    = fromRight $
                        mkScheduleFromEffectiveDate
                            Settings.defaultSettings
                            begin
                            end
                            calendarUKLSE
                            Following
                            Nothing
                            tenor
                            (Just Forward)
                            (Just False)
                            NullDate
                            ntl
          begin    = fromJust $ mkDate 01 01 2017
          end      = fromJust $ mkDate 09 01 2017
          ntl      = fromJust $ mkDate 08 20 2017
          tenor    = Just $ mkPeriodFromTime 1 Months

schedule_forwards_regular
    = testCase "Schedule Forwards Regular Test" $ assertEqual "" expected actual
    where actual = regular sched
          expected = regular_forwards_expected
          sched    = fromRight $
                        mkScheduleFromEffectiveDate
                            Settings.defaultSettings
                            begin
                            end
                            calendarUKLSE
                            Following
                            Nothing
                            tenor
                            (Just Forward)
                            (Just False)
                            NullDate
                            ntl
          begin    = fromJust $ mkDate 01 01 2017
          end      = fromJust $ mkDate 09 01 2017
          ntl      = fromJust $ mkDate 08 20 2017
          tenor    = Just $ mkPeriodFromTime 1 Months
      
date1 = fromJust $ mkCalDate 01 01 2017
datelist_test1
    =  fmap (fromJust . uncurry3 mkCalDate)
        [(1,1,2017),(2,1,2017),(3,1,2017),(4,1,2017),(5,1,2017),(6,1,2017),(7,1,2017)]

date2 = fromJust $ mkCalDate 07 01 2017

scheddates
    =  fmap (fromJust . uncurry3 mkDate)
        [(1,1,2017),(2,1,2017),(3,1,2017),(4,1,2017),(5,1,2017),(6,1,2017),(7,1,2017),(8,1,2017),(9,1,2017)]

scheddates_unadjusted
    =  fmap (fromJust . uncurry3 mkDate)
        [(1,1,2017),(1,20,2017),(2,20,2017),(3,20,2017),(4,20,2017),(5,20,2017),(6,20,2017),(7,20,2017),(8,20,2017),(9,1,2017)]

scheddates_adjusted
    =  fmap (fromJust . uncurry3 mkDate)
        [(1,3,2017),(1,20,2017),(2,20,2017),(3,20,2017),(4,20,2017),(5,22,2017),(6,20,2017),(7,20,2017),(8,21,2017),(9,1,2017)]

scheddates_forward_adjusted
    =  fmap (fromJust . uncurry3 mkDate)
            [(1,3,2017),(2,1,2017),(3,1,2017),(4,3,2017),
             (5,2,2017),(6,1,2017),(7,3,2017),
             (8,1,2017),(8,21,2017),(9,1,2017)]
    

regular_expected
    = [False,True,True,True,True,True,True,True,False]

regular_forwards_expected
    = [True,True,True,True,True,True,True,False,False]    
-- utils

fromRight :: Either String b -> b
fromRight (Right b) = b
fromRight (Left s)  = error $ "Left: " ++ s

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
