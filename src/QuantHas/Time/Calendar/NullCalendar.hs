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

module QuantHas.Time.Calendar.NullCalendar (nullCalendar, module QuantHas.Time.Calendar.Calendar) where

import QuantHas.Time.Calendar.Calendar
import QuantHas.Time.Calendar.CalendarImpl
import QuantHas.Time.Date
import QuantHas.Time.DayName

nullCalendar :: Calendar
nullCalendar
    = mkCalendar "Null" isBusinessDayNC isWeekendNC isHolidayNC undefined

-- always return True
isBusinessDayNC :: DatePred
isBusinessDayNC = const True

isHolidayNC :: DatePred
isHolidayNC = const False

isWeekendNC :: DatePred
isWeekendNC = const False
