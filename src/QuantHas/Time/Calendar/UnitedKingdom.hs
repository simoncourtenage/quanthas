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
isBusinessDayUK :: DatePred
isBusinessDayUK NullDate = Left "Cannot determine business day from null date"
isBusinessDayUK date@(Date day month year serial)
    = (isWeekendUK date)
        >>= (\b -> isNewYearsDay day month >>= \b' -> Right $ b || b')
        >>= (\b -> Right $ b || dayOfYear == easterMonday)
        -- good Friday?
        >>= (\b -> Right $ b || dayOfYear == easterMonday - 3)
        -- first or last bank holiday?
        >>= (\b -> fmap (\d -> b || (month == 5 && d == Monday && (day <= 7 || (day >= 25 && year /= 2002)))) weekday)
        -- august bank holiday?
        >>= (\b -> fmap (\d -> b || (month == 7 && d == Monday && day >= 25)) weekday)
        -- Christmas?
        >>= (\b -> Right $ b || (month == 12 && day == 25))
        -- if Xmas falls on Saturday or Sunday
        >>= (\b -> fmap (\d -> b || (month == 12 && day == 27 && (d == Monday || d == Tuesday))) weekday) 
        -- same as above
        >>= (\b -> fmap (\d -> b || (month == 12 && day == 28 && (d == Monday || d == Tuesday))) weekday) 
        -- Golden Jubilee bank holiday
        >>= (\b -> Right $ b || ((month == 6 && year == 2002 && (day == 3 || day == 4)))) 
        -- end of millenium
        >>= (\b -> Right $ b || (year == 1999 && month == 12 && day == 31))
    where weekday                 = getweekdayname date
          dayOfYear               = dayOfTheYear serial
          easterMonday            = ciGetEasterMonday westernCalendarImpl year
          isNewYearsDay day month = fmap (\d -> month == 1 && (day == 1 || (day <= 3 && d == Monday))) weekday

-- TO DO          
isHolidayUK :: DatePred
isHolidayUK d = Right True

isWeekendUK :: DatePred
isWeekendUK = fmap isWesternWeekend . getweekdayname
