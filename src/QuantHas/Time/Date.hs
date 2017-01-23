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

module QuantHas.Time.Date ( module QuantHas.Time.TimeUnit, module QuantHas.Time.Date) where

import QuantHas.Time.TimeUnit
import QuantHas.Time.Period

type Day = Int
type Month = Int
type Year = Int
type SerialNumber = Int

data DayName =  Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Eq, Show, Enum)

-- store all the components of a date precomputed
data Date = Date { getday::Day, getmonth::Month, getyear::Year, getserialnumber::SerialNumber }
            deriving (Eq)

-- define a custom version of show for Date objects
instance Show Date where
    show (Date 0 0 0 0)               = "Null Date"
    show (Date day month year serial) = (displayDay day) ++ " " ++ (displayMonth month) ++ " " ++ show year

-- logical comparison operators - delegate to comparison of the serial numbers
instance Ord Date where
    (<) (Date d1 m1 y1 s1) (Date d2 m2 y2 s2) = s1 < s2
    (<=) (Date d1 m1 y1 s1) (Date d2 m2 y2 s2) = s1 <= s2
    (>) (Date d1 m1 y1 s1) (Date d2 m2 y2 s2) = s1 > s2
    (>=) (Date d1 m1 y1 s1) (Date d2 m2 y2 s2) = s1 >= s2
    
    
displayDay :: Day -> String
displayDay day = show day ++ daysuffix day

daysuffix day | elem day [1,21,31] = "st"
daysuffix day | elem day [2,22]    = "nd"
daysuffix day | elem day [3,23]    = "rd"
daysuffix day | otherwise          = "th"
          
displayMonth :: Month -> String
displayMonth month = monthname !! (month - 1)
    where monthname = ["January","February","March","April","May","June","July",
                       "August","September","October","November","December"]

-- constructor functions                       

makeDate:: Day -> Month -> Year -> Date
makeDate day month year = Date day month year (makeSerialNumber day month year)

makeDateFromSerial serial = Date day month year serial
    where year  = calcyear serial
          monthlst = if isLeapYear year then monthOffsetLeapLst else monthOffsetLst
          dayofyear = dayOfTheYear serial
          month = calculateMonth dayofyear monthlst
          day = dayofyear - (monthlst !! (month - 1))

-- QL Date class includes an empty constructor and some code (e.g, Schedule class) uses this
-- constructor.  In this first pass of creatin QuantHas from the QL code, we include this
-- constructor to allow transcription to continue but we should seek to eliminate it in the future
mkNullDate:: Date
mkNullDate = Date 0 0 0 0

-- advance a date by a number of specified timeunits
advance:: Date -> Int -> TimeUnit -> Date
advance (Date _ _ _ serial) num Days   = makeDateFromSerial (serial + num)
advance (Date _ _ _ serial) num Weeks  = makeDateFromSerial (serial + num*7)
advance (Date d m y serial) num Months
    = Date newday newmonth newyear (makeSerialNumber newday newmonth newyear)
    where
    (newmonth,newyear) = adjustmnthyr (m + num) y
    mnthlen            = monthLength newmonth (isLeapYear newyear)
    newday             = if d > mnthlen then mnthlen else d

adjustmnthyr:: Month -> Year -> (Month,Year)
adjustmnthyr mnth yr | mnth < 1  = adjustmnthyr (mnth+12) (yr-1)
                     | mnth > 12 = adjustmnthyr (mnth-12) (yr+1)
                     | otherwise = (mnth,yr)

addToDate:: Date -> Period -> Date
addToDate date (Period num tunit _) = advance date num tunit

subtractFromDate :: Date -> Period -> Date
subtractFromDate date (Period num tunit _) = advance date (-num) tunit

-- assumes that the first date arg is later than the second date arg
-- (as the Quantlib operator- does)
subtractDates :: Date -> Date -> Int
subtractDates (Date _ _ _ serial1) (Date _ _ _ serial2) = serial1 - serial2
                     
makeSerialNumber d m y = d + (monthOffset m (isLeapYear y)) + (yearOffset y)



-- day functions
              
dayOfTheYear :: SerialNumber -> Int
dayOfTheYear serial = serial - yearOffset(calcyear serial)

-- returns a value in the range 1 (Sunday) - 7 (Saturday) corresponding to the position of the day
-- in the week
weekday :: Date -> Int
weekday (Date _ _ _ serial)
    = let dayno = serial `mod` 7
      in if dayno == 0 then 7 else dayno
      
-- returns the name of the day of week
getweekdayname :: Date -> DayName
getweekdayname = toEnum . pred . weekday


-- month functions
              
monthLength:: Month -> Bool -> Int
monthLength month isleap = if isleap && month == 2 then 29 else monthDays !! (month - 1) 

calculateMonth:: Int -> [Int] -> Month
calculateMonth daynum monthlst = length (takeWhile (< daynum) monthlst)

-- is the given date the last day of the month?
isEndOfMonth :: Date -> Bool
isEndOfMonth (Date day month year serial)
    = let monthlens = if (isLeapYear year) then monthDaysLeap else monthDays
      in day == monthlens !! (month - 1)
      
-- get the date corresponding to the end of the month for a given date
endOfMonth :: Date -> Date
endOfMonth (Date day month year _)
    = Date lastday month year (makeSerialNumber lastday month year)
    where lastday = monthlens !! (month - 1)
          monthlens = if isLeapYear year then monthDaysLeap else monthDays

monthDays :: [Int]
monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

monthDaysLeap :: [Int]
monthDaysLeap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]


monthOffset :: Month -> Bool -> Int
monthOffset month isleap = if isleap then monthOffsetLeapLst !! (month -1) else monthOffsetLst !! (month - 1)

monthOffsetLst :: [Int]
monthOffsetLst  = [0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365]

monthOffsetLeapLst :: [Int]
monthOffsetLeapLst = [0,  31,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, 366]

-- year functions

-- 1900 is not a leap year, but quantlib's isLeap function treats it as one in line with
-- an apparent bug in excel
isLeapYear :: Year -> Bool
isLeapYear year = if (year /= 1900) then year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
              else True
              
-- calculate the year from the serial number
calcyear :: SerialNumber -> Year
calcyear serial = findyear 0 yearOffsetLst
    where findyear yrnum (y:ys)
            | y >= serial  = yrnum + 1900 - 1
            | otherwise   = findyear (yrnum + 1) ys


yearOffset :: Year -> Int
yearOffset year = yearOffsetLst !! (year - 1900)

-- quantlib has year offset values (which are number of days from 1 jan 1900 to 21st dec of prev year inclusive)
-- precomputed up to 2200, but let's try an infinite list!
yearOffsetLst :: [Int]
yearOffsetLst = 0 : yearOffsetLst' 0 1901
    where yearOffsetLst' prev year = let newdays = upddays prev year
                                     in newdays : yearOffsetLst' newdays (year + 1)
          upddays prev year        = if isLeapYear (year - 1) then prev + 366 else prev + 365

