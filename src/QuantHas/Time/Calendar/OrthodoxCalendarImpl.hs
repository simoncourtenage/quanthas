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
  Module: QuantHas.Time.Calendar.CalendarImpl
  Description: Orthodox Calendar Implementation data declaration
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
-}

module QuantHas.Time.Calendar.OrthodoxCalendarImpl
    (orthodoxCalendarImpl
   , isOrthodoxEaster
   , isOrthodoxWeekend
   , getOrthodoxEasterMonday
   , module QuantHas.Time.Calendar.CalendarImpl)
    where

import QuantHas.Time.Calendar.CalendarImpl
import QuantHas.Time.DayName
import Data.Array

orthodoxCalendarImpl = CalImpl isOrthodoxEaster getOrthodoxEasterMonday isOrthodoxWeekend orthodoxEasterMondays

-- | Is the given day in the year an Easter Monday according to the Orthodox Calendar?
isOrthodoxEaster :: Int -> Year -> Bool
isOrthodoxEaster = checkIsEasterMonday orthodoxCalendarImpl

-- | get easter monday from orthodox calendar 
getOrthodoxEasterMonday :: Year -> Int
getOrthodoxEasterMonday = lookupEasterMonday orthodoxCalendarImpl

isOrthodoxWeekend :: DayName -> Bool
isOrthodoxWeekend Saturday = True
isOrthodoxWeekend Sunday   = True
isOrthodoxWeekend _        = False

-- | Western calendar Easter  - taken from Calendar.cpp in Quantlib

orthodoxEasterMondaysLst :: [Int]
orthodoxEasterMondaysLst
    = [
                  98,  90, 103,  95, 114, 106,  91, 111, 102,   -- 1901-1909
             87, 107,  99,  83, 103,  95, 115,  99,  91, 111,   -- 1910-1919
             96,  87, 107,  92, 112, 103,  95, 108, 100,  91,   -- 1920-1929
            111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   -- 1930-1939
             85, 104,  96, 116, 101,  92, 112,  97,  89, 108,   -- 1940-1949
            100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   -- 1950-1959
            109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   -- 1960-1969
             89, 102,  94, 113, 105,  90, 110, 101,  86, 106,   -- 1970-1979
             98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   -- 1980-1989
            106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   -- 1990-1999
            115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   -- 2000-2009
             95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   -- 2010-2019
            104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   -- 2020-2029
            112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   -- 2030-2039
             93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   -- 2040-2049
            101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   -- 2050-2059
            110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   -- 2060-2069
             90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   -- 2070-2079
             99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   -- 2080-2089
            107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   -- 2090-2099
             88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   -- 2100-2109
             97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   -- 2110-2119
            106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   -- 2120-2129
             86, 106,  98, 110, 102,  94, 114,  98,  90, 110,   -- 2130-2139
             95,  86, 106,  91, 111, 102,  94, 107,  99,  90,   -- 2140-2149
            103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   -- 2150-2159
             84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   -- 2160-2169
             92, 112, 104,  95, 108, 100,  92, 111,  96,  88,   -- 2170-2179
            108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   -- 2180-2189
            116, 101,  93, 112,  97,  89, 109, 100,  85, 105    -- 2190-2199
      ]

-- | Builds the array of days representing the position of Easter Monday in the Western calendar each
--  year.  The year 1901 is at position 1 in the array (to facilitate arithmetic, such
--  as (year - 1900) to calculate array indices.
orthodoxEasterMondays :: Array Int Int
orthodoxEasterMondays
    = convEasterMondays orthodoxEasterMondaysLst