{-
    Copyright (C) 2010, Simon Courtenage (courtenage@gmail.com)
    
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
  Module: QuantHas.Time.Calendar.Functions
  Description: Functions over Calendar and Calendar dates
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
-}

module QuantHas.Time.Calendar.Functions
    (module QuantHas.Time.Calendar.Functions)
    where

import Data.Bool (bool)
import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Date
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.Period

-- | advance a date by a specified amount of time (e.g, 1 week, or 7 days).  If the unit of time to advance
-- is Months or Years, then we need to worry about ends of months.  If the given date is an end of month date,
-- and we want the returned date to be an end of month (indicated by end of month flag), then we need the
-- end of month for the month of the advanced date.  E.g., if we are given 30th September, then 1 month on
-- is 31st October, but the end of month business day may be 29th October.
advanceDateByUnit ::
     Calendar
  -> CalDate               -- ^ which date to advance from
  -> Int                   -- ^ how much to advance by
  -> TimeUnit              -- ^ time units to advance by
  -> BusinessDayConvention -- ^ how dates should be adjusted if not a business day
  -> Bool                  -- ^ end of month flag
  -> CalDate
advanceDateByUnit cal d n unit c eom
    | n == 0        = adjustCalendarDate cal d c
    | unit == Weeks = adjustCalendarDate cal (addToDate d (mkPeriod (PeriodTimeArgs n Weeks))) c
    | unit == Days  = if (n > 0) then
                        advanceDays d n ((flip addToDate) (mkPeriodFromTime 1 Days)) (subtract 1)
                     else
                        advanceDays d n ((flip subtractFromDate) (mkPeriodFromTime 1 Days))  ((+)1)
    | otherwise     = if (eom && isEndOfMonth d) then
                        endOfMonthBusinessDay cal d1
                      else
                        adjustCalendarDate cal d1 c
                      where
                        d1                     = addToDate d (mkPeriod (PeriodTimeArgs n unit))
                        advanceDays d 0 _ _    = d
                        advanceDays d n fd fn  = advanceDays (fd $ advanceIfHoliday d fd) (fn n) fd fn
                        advanceIfHoliday d fd  = bool d (advanceIfHoliday (fd d) fd) (isHoliday cal d)

advanceDateByPeriod :: Calendar -> CalDate -> Period -> BusinessDayConvention -> Bool -> CalDate
advanceDateByPeriod cal d p bdc eom 
   = advanceDateByUnit cal d (lenPeriod p) (units p) bdc eom

adjustCalendarDate :: Calendar -> CalDate -> BusinessDayConvention -> CalDate
adjustCalendarDate cal d Unadjusted = d
adjustCalendarDate cal d Nearest    = if (isHoliday cal d1) then d2 else d1
                                       where
                                       (d1,d2) = untilHoliday cal (d,d)
                                       untilHoliday c (d1',d2')
                                        | (not $ isHoliday c d1') && (not $ isHoliday c d2')
                                                    = untilHoliday c (addToDate d1' (mkPeriodFromTime 1 Days),subtractFromDate d2' (mkPeriodFromTime 1 Days))
                                        | otherwise = (d1',d2')
adjustCalendarDate cal d Following
    = whileHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))
    
adjustCalendarDate cal d ModifiedFollowing
    = if getmonth d1 /= getmonth d then
        adjustCalendarDate cal d Preceding
      else
        d1
      where
      d1 = whileHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d HalfMonthModifiedFollowing
   = if (getmonth d1 /= getmonth d) || (getday d <= 15 && getday d1 > 15) then
        adjustCalendarDate cal d Preceding
      else
        d1
      where
      d1 = whileHoliday cal d ((flip addToDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d Preceding
    = whileHoliday cal d ((flip subtractFromDate) (mkPeriodFromTime 1 Days))

adjustCalendarDate cal d ModifiedPreceding
    = if getmonth d1 /= getmonth d then
        adjustCalendarDate cal d Following
      else
        d1
      where
      d1 = whileHoliday cal d ((flip subtractFromDate) (mkPeriodFromTime 1 Days))


whileHoliday :: Calendar -> CalDate -> (CalDate -> CalDate) -> CalDate
whileHoliday cal d f | isHoliday cal d = whileHoliday cal (f d) f
                     | otherwise       = d

endOfMonthBusinessDay :: Calendar -> CalDate -> CalDate
endOfMonthBusinessDay cal d = adjustCalendarDate cal (endOfMonth d) Preceding

