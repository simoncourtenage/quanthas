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

module QuantHas.Time.Date
  (module QuantHas.Time.TimeUnit
 , module QuantHas.Time.Date
 , module QuantHas.Time.DayName)
  where

import QuantHas.Time.TimeUnit
import QuantHas.Time.Period
import QuantHas.Time.DayName
import Control.Applicative

type Day = Int
type Month = Int
type Year = Int
type SerialNumber = Int

{--
  How to deal with null dates?
--}

-- store all the components of a date precomputed
data Date = Date { getmonth::Month, getday::Day, getyear::Year, getserial::SerialNumber } | NullDate
            deriving (Eq)

-- define a custom version of show for Date objects
instance Show Date where
    show NullDate                     = "Null Date"
    show (Date m d y serial) = (displayDay d) ++ " " ++ (displayMonth m) ++ " " ++ show y

-- logical comparison operators - delegate to comparison of the serial numbers
instance Ord Date where
    (<) d1 d2 = getserial d1 < getserial d2
    (<=) d1 d2 = getserial d1 <= getserial d2
    (>) d1 d2 = getserial d1 > getserial d2
    (>=) d1 d2 = getserial d1 >= getserial d2 

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

-- | Create date given month, day, year.
--   Using the american order (month, day, year) allows us to validate
--   month before using it to validate number of days
mkDate:: Month -> Day -> Year -> Maybe Date
mkDate m d y = Date <$> (isValidMonth m )
                        <*> (isValidDay m d)
                        <*> (isValidYear y)
                        <*> (Just $ makeSerialNumber d m y)

isValidDay :: Month -> Day -> Maybe Day
isValidDay m d | m > 0 && m < 13 && d > 0 && d <= ds = Just d
               | otherwise       = Nothing
  where ds = monthDays !! (m - 1)

isValidMonth :: Month -> Maybe Month
isValidMonth m | m > 0 && m < 13 = Just m
               | otherwise       = Nothing

-- Don't accept years before 1901
isValidYear :: Year -> Maybe Year
isValidYear y | y >= 1901 = Just y
              | otherwise = Nothing

-- We assume that serial number counting begins from 1901
mkDateFromSerial :: SerialNumber -> Date
mkDateFromSerial 0 = NullDate
mkDateFromSerial s = Date m d y s
    where y  = calcyear s
          ml = if isLeapYear y then monthOffsetLeapLst else monthOffsetLst
          doy = dayOfTheYear s
          m = calculateMonth doy ml
          d = doy - (ml !! (m - 1))

-- QL Date class includes an empty constructor and some code (e.g, Schedule class) uses this
-- constructor.  In this first pass of creatin QuantHas from the QL code, we include this
-- constructor to allow transcription to continue but we should seek to eliminate it in the future
mkNullDate:: Date
mkNullDate = NullDate

isNullDate :: Date -> Bool
isNullDate NullDate = True
isNullDate d        = False

-- advance a date by a number of specified timeunits
advance:: Date -> Int -> TimeUnit -> Date
advance d num Days   = mkDateFromSerial (getserial d + num)
advance d num Weeks  = mkDateFromSerial (getserial d + num*7)
advance d num Months
    = Date newmonth newday newyear (makeSerialNumber newday newmonth newyear)
    where
    (newmonth,newyear) = adjustmnthyr (getmonth d + num) (getyear d) 
    mnthlen            = monthLength newmonth (isLeapYear newyear)
    newday             = if getday d > mnthlen then mnthlen else getday d

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
subtractDates d1 d2 = getserial d1 - getserial d2
                     
makeSerialNumber d m y = d + (monthOffset m (isLeapYear y)) + (yearOffset y)



-- day functions
              
dayOfTheYear :: SerialNumber -> Int
dayOfTheYear serial = serial - yearOffset(calcyear serial)

-- returns a value in the range 1 (Sunday) - 7 (Saturday) corresponding to the position of the day
-- in the week
weekday :: Date -> Either String Int
weekday NullDate = Left "Cannot calculate weekday from null date"
weekday d
    = let dayno = getserial d `mod` 7
      in if dayno == 0 then Right 7 else Right dayno
      
-- returns the name of the day of week
getweekdayname :: Date -> Either String DayName
getweekdayname d = weekday d >>= \x -> Right $ (toEnum . pred) x


-- month functions
              
monthLength:: Month -> Bool -> Int
monthLength month isleap = if isleap && month == 2 then 29 else monthDays !! (month - 1) 

calculateMonth:: Int -> [Int] -> Month
calculateMonth daynum monthlst = length (takeWhile (< daynum) monthlst)

-- is the given date the last day of the month?
isEndOfMonth :: Date -> Bool
isEndOfMonth d
    = let monthlens = if (isLeapYear $ getyear d) then monthDaysLeap else monthDays
      in getday d == monthlens !! (getmonth d - 1)
      
-- get the date corresponding to the end of the month for a given date
endOfMonth :: Date -> Date
endOfMonth d
    = Date (getmonth d) lastday (getyear d) (makeSerialNumber lastday (getmonth d) (getyear d))
    where lastday = monthlens !! (getmonth d - 1)
          monthlens = if isLeapYear $ getyear d then monthDaysLeap else monthDays

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
calcyear s = findyear 0 yearOffsetLst
    where findyear yrnum (y:ys)
            | y >= s  = yrnum + 1900 - 1
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

