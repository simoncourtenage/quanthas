{-
    Copyright (C) 2010, Simon Courtenage (courtenage@gmail.com)
    
    This file is part of QuantHas, an open-source Haskell implementation
    of the QuantLib library for quantitative finance.
    
    QuantHas is free software: you can redistribute it and/or modify it
    under the terms of the QuantHas license.  You should have received a
    copy of the license along with this program.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the license for more details.
    
-}

module QuantHas.Time.Calendar.UnitedKingdom
    (module QuantHas.Time.Calendar.Calendar
   , calendarUKLSE)
where

import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Calendar.WesternCalendarImpl
import QuantHas.Time.Date

calendarUKLSE :: Calendar
calendarUKLSE
    = mkCalendar "London Stock Exchange" isBusinessDayUK isWeekendUK isHolidayUK westernCalendarImpl

-- | The Quantlib UK calendar has three different implementations of isBusinessDay
--  for the three different markets (Settlement, Exchange and Metals) - however, all
--  three functions are exactly the same, so here we only provide one.
--  We assume that null dates have already been dealt with.
--  The original QL code has isHoliday defined as not isBusinessDay, but the code for isBusinessDay first works
--  out if a date is holiday and then returns True if it isn't.  In other words, the definition of a holiday is in
--  the function for business days, and not in the function for holidays.  What we do here is hopefully a little more
--  straightforward.
isBusinessDayUK :: DatePred
isBusinessDayUK = not . isHolidayUK

-- helper functions

-- | List of predicates for UK holidays.  If a new holiday needs to be added,
--   create a new predicate and add it here.
ukholidays :: [DatePred]
ukholidays = 
    [
        isWeekendUK,
        isNewYearsDay,
        isEasterDate,
        isMayBankHoliday,
        isGoldenJubileeBankHoliday,
        isAugustBankHoliday,
        isChristmas,
        isBoxingDay,
        isEndOfMillenium
    ]

isNewYearsDay :: DatePred
isNewYearsDay date@(CalDate d m _ _) = m == 1 && (d == 1 || (d <= 3 && getweekdayname date == Monday))

-- | Is the date Easter Monday or Good Friday
isEasterDate :: DatePred
isEasterDate d = doy == em || doy == em - 3
    where doy = dayOfTheYear $ getserial d
          em = ciGetEasterMonday westernCalendarImpl . getyear $ d

-- | Is date one of the two bank holidays in May
--   There was no late bank holiday in 2002 because of the Golden Jubilee bank holiday on June 3rd
--   See https://www.timeanddate.com/holidays/uk/2002
isMayBankHoliday :: DatePred
isMayBankHoliday date@(CalDate d m y _)
    = m == 5 && (d <= 7 || (d >= 25 && y /= 2002)) && getweekdayname date == Monday

-- | Is the golden jubilee bank holiday (3rd June 2002) or the early spring bank holiday that followed
--   on the 4th?
isGoldenJubileeBankHoliday :: DatePred
isGoldenJubileeBankHoliday (CalDate d m y _) = y == 2002 && m == 6 && (d == 3 || d == 4)

isAugustBankHoliday :: DatePred
isAugustBankHoliday date@(CalDate d m _ _)
    = m == 8 && d >= 25 && getweekdayname date == Monday

-- | If Christmas falls on a weekend, then you get the Monday or Tuesday off
--   See, for example, https://www.timeanddate.com/holidays/uk/2004.  Christmas was on a Saturday
--   and, hence, as a holiday, was observed on the following Tuesday.
isChristmas :: DatePred
isChristmas date@(CalDate d m _ _)
    = m == 12 &&
        (d == 25 || (d == 27 && (wd == Monday || wd == Tuesday)))
    where wd = getweekdayname date

-- | The holiday after Christmas day
isBoxingDay :: DatePred
isBoxingDay date@(CalDate d m _ _)
    = m == 12 &&
        (d == 26 || (d == 28 && (wd == Monday || wd == Tuesday)))
    where wd = getweekdayname date

-- | Sounds apocalyptic, but not really
isEndOfMillenium :: CalDate -> Bool
isEndOfMillenium (CalDate 31 12 1999 _) = True
isEndOfMillenium _                   = False
          
isHolidayUK :: DatePred
isHolidayUK d = foldr (||) False $ map ($ d) ukholidays 

isWeekendUK :: DatePred
isWeekendUK = isWesternWeekend . getweekdayname
