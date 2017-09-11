{-
    Copyright (C) 2017, Simon Courtenage (courtenage@gmail.com)
    
    This file is part of QuantHas, an open-source Haskell implementation
    of the QuantLib library for quantitative finance.
    
    Quanthas is free software: you can redistribute it and/or modify it
    under the terms of the QuantHas license.  You should have received a
    copy of the license along with this program.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the license for more details.
    
-}

{-|
  Module: QuantHas.Time.Calendar
  Description: Calendar data declaration and functions over calendars
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
-}
module QuantHas.Time.Calendar.Calendar
  (Calendar
   , mkCalendar
   , isBusinessDay
   , isWeekend
   , isHoliday
   , getEasterMonday
  )
where

import QuantHas.Time.Calendar.CalendarImpl
import Data.Array
import QuantHas.Time.Date
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.Period

newtype Calendar = Calendar Cal

data Cal = Cal
           {
               calendarName :: String,
               calendarIsBusinessDay :: Date -> Bool, -- ^ function to determine if date is a business day
               calendarIsWeekend :: Date -> Bool,     -- ^ function to determine if date is weekend in calendar
               calendarIsHoliday :: Date -> Bool,     -- ^ is date a holiday?
               calendarImpl :: CalImpl                -- ^ calendar representation
           }


-- main functions over calendars

isBusinessDay :: Calendar -> Date -> Bool
isBusinessDay (Calendar cal) = calendarIsBusinessDay cal

isWeekend :: Calendar -> Date -> Bool
isWeekend (Calendar cal) = calendarIsWeekend cal

isHoliday :: Calendar -> Date -> Bool
isHoliday (Calendar cal) = calendarIsHoliday cal

getEasterMonday :: Calendar -> Int -> Int
getEasterMonday (Calendar c) = (ciGetEasterMonday . calendarImpl) c

-- | make a calendar for a specific country
mkCalendar :: String -> (Date -> Bool) -> (Date -> Bool) -> (Date -> Bool) -> CalImpl -> Calendar
mkCalendar name bus wkend hols calimpl = Calendar $ Cal name bus wkend hols calimpl

advanceDateByUnit :: Calendar -> Date -> Int -> TimeUnit -> BusinessDayConvention -> Bool -> Date
advanceDateByUnit cal d n unit c endOfMonth
    | isNullDate d  = error "Calendar::advanceDateByUnit - null date"
    | n == 0        = adjustCalendarDate cal d c
    | unit == Weeks = adjustCalendarDate cal (addToDate d (mkPeriod (PeriodTimeArgs n Weeks))) c
    | unit == Days  = if (n > 0) then
                        advanceDays d n ((flip addToDate) (mkPeriodFromTime 1 Days)) (subtract 1)
                     else
                        advanceDays d n ((flip subtractFromDate) (mkPeriodFromTime 1 Days))  ((+)1)
    | otherwise     = if (endOfMonth && isEndOfMonth d) then calcEndOfMonth cal d1 else adjustCalendarDate cal d1 c
                      where
                        d1                        = addToDate d (mkPeriod (PeriodTimeArgs n unit))
                        advanceDays d' 0 _ _      = d'
                        advanceDays d' n fd fn    = advanceDays (advanceIfHoliday (fd d) fd) (fn n) fd fn
                        advanceIfHoliday d'' fd'' = if isHoliday cal d'' then
                                                        advanceIfHoliday (fd'' d'') fd''
                                                    else d''

advanceDateByPeriod :: Calendar -> Date -> Period -> BusinessDayConvention -> Bool -> Date
advanceDateByPeriod cal d p bdc eom 
   = advanceDateByUnit cal d (lenPeriod p) (units p) bdc eom

adjustCalendarDate :: Calendar -> Date -> BusinessDayConvention -> Date
adjustCalendarDate cal d Unadjusted = d
adjustCalendarDate cal d Nearest    = if (isHoliday cal d1) then d2 else d1
                                       where
                                       (d1,d2) = untilHoliday cal (d,d)
                                       untilHoliday c (d1',d2')
                                        | (not $ isHoliday c d1') && (not $ isHoliday c d2')
                                                    = untilHoliday c (addToDate d1' (mkPeriodFromTime 1 Days),subtractFromDate d2' (mkPeriodFromTime 1 Days))
                                        | otherwise = (d1',d2')
adjustCalendarDate cal d Following
    = untilHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))
    
adjustCalendarDate cal d ModifiedFollowing
    = if getmonth d1 /= getmonth d then
        adjustCalendarDate cal d Preceding
      else
        d1
      where
      d1 = untilHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d HalfMonthModifiedFollowing
   = if (getmonth d1 /= getmonth d) || (getday d <= 15 && getday d1 > 15) then
        adjustCalendarDate cal d Preceding
      else
        d1
      where
      d1 = untilHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d Preceding
    = untilHoliday cal d ((flip subtractFromDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d ModifiedPreceding
    = if getmonth d1 /= getmonth d then
        adjustCalendarDate cal d Following
      else
        d1
      where
      d1 = untilHoliday cal d ((flip subtractFromDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate _ _ _ = error "adjustCalendarDate: unknown BusinessDayConvention" 

untilHoliday :: Calendar -> Date -> (Date->Date) -> Date
untilHoliday cal d f | isHoliday cal d = f d
                     | otherwise       = d

calcEndOfMonth :: Calendar -> Date -> Date
calcEndOfMonth cal d = adjustCalendarDate cal (endOfMonth d) Preceding
