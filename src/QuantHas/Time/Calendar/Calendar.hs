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
   , DatePred
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

instance Show Calendar where
  show (Calendar c) = show c

type DatePred = CalDate -> Bool

data Cal = Cal
           {
               calendarName :: String,
               calIsBusinessDay :: DatePred, -- ^ function to determine if date is a business day
               calIsWeekend :: DatePred,     -- ^ function to determine if date is weekend in calendar
               calIsHoliday :: DatePred,     -- ^ is date a holiday?
               calImpl :: CalImpl            -- ^ calendar representation
           }

instance Show Cal where
  show c = show $ "Calendar " ++ calendarName c


-- main functions over calendars

-- | Is the date a business day.
--   Note that this function is responsible for filtering out null dates
isBusinessDay :: Calendar -> Date -> Either String Bool
isBusinessDay _ NullDate = Left "Cannot determine business day from null date"
isBusinessDay (Calendar c) d = Right . calIsBusinessDay c . getCalDate $ d

isWeekend :: Calendar -> Date -> Either String Bool
isWeekend _ NullDate = Left "Cannot determine weekend from null date"
isWeekend (Calendar c) d = Right . calIsWeekend c . getCalDate $ d

isHoliday :: Calendar -> Date -> Either String Bool
isHoliday _ NullDate = Left "Cannot determine holiday from null date"
isHoliday (Calendar c) d = Right . calIsHoliday c . getCalDate $ d

getEasterMonday :: Calendar -> Int -> Int
getEasterMonday (Calendar c) = (ciGetEasterMonday . calImpl) c

-- | make a calendar for a specific country
mkCalendar :: String -> DatePred -> DatePred -> DatePred -> CalImpl -> Calendar
mkCalendar name bus wkend hols calimpl = Calendar $ Cal name bus wkend hols calimpl



