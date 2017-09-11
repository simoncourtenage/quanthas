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
isBusinessDayUK :: Date -> Bool
isBusinessDayUK date@(Date day month year serial)
    = not (isWeekendUK date || isNewYearsDay day month
            || dayOfYear == easterMonday || dayOfYear == easterMonday - 3 -- i.e., is it good friday
            || (month == 5 && weekday == Monday
                    && (day <= 7 || (day >= 25 && year /= 2002))) -- first or last bank holiday in May
            || (month == 7 && weekday == Monday && day >= 25) -- august bank holiday
            || (month == 12 && day == 25) -- Christmas
            || (month == 12 && day == 27 && (weekday == Monday || weekday == Tuesday)) -- if Xmas is Sat/Sun
            || (month == 12 && day == 28 && (weekday == Monday || weekday == Tuesday)) -- if Xmas is Sat/Sun
            || (month == 6 && year == 2002 && (day == 3 || day == 4)) -- Golden Jubilee bank holidays
            || (year == 1999 && month == 12 && day == 31)) -- end of millenium
    where weekday = getweekdayname date
          dayOfYear               = dayOfTheYear serial
          easterMonday            = ciGetEasterMonday westernCalendarImpl year
          isNewYearsDay day month = month == 1 && (day == 1 || (day <= 3 && weekday == Monday))
          
isHolidayUK :: Date -> Bool
isHolidayUK date = True

isWeekendUK :: Date -> Bool
isWeekendUK = isWesternWeekend . getweekdayname
