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
  Module: QuantHas.Time.Schedule
  Description: Schedule represents a sequence of coupon dates from a calendar
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
  Status: work in progress
-}

module QuantHas.Time.Schedule(module QuantHas.Time.Schedule) where

import Data.Array
import Data.Maybe
import Data.Bool (bool)
import QuantHas.Time.Date
import QuantHas.Time.Calendar.NullCalendar
import QuantHas.Time.Calendar.Functions
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.DateGeneration
import QuantHas.Time.Period
import QuantHas.Time.IMM (isIMMdate)
import QuantHas.Settings

{--
  Draft commentary
  ----------------
  Data type: This is pretty much a faithful representation of the Schedule class in the QL code.
  TO DO - do we need to hold all this data?  What parts are just used to calculate the
  coupon dates and hence don't need to be kept?

  Functions: this is work in progress!
--}

data Schedule = Schedule
				{
					dates :: [Date],
					calendar :: Calendar,
					convention :: BusinessDayConvention,
					termDateConvention:: Maybe BusinessDayConvention,
					tenor :: Maybe Period,
					rule :: Maybe DateGenerationRule,
					useEndOfMonth :: Maybe Bool,
					firstDate :: Date,
					nextToLastDate :: Date,
					regular :: [Bool]
				}
        deriving (Show)

-- | Create a schedule from pre-prepared values
mkSchedule ::
     [Date]                          -- ^ coupon dates
  -> Calendar                        -- ^ calendar used in date calculations
  -> BusinessDayConvention           -- ^
  -> Maybe BusinessDayConvention     -- ^ business day convention for termination dates
  -> Maybe Period                    -- ^ tenor
  -> Maybe DateGenerationRule        -- ^ how dates are to be generated in calculating dates for schedule
  -> Maybe Bool                      -- ^ End of month
  -> [Bool]                          -- ^ Boolean values for dates indicating whether regular or not
  -> Either String Schedule
mkSchedule d c conv tconv t r eom reg
  | length reg == 0 || length reg == length d - 1
    = Right $ mkSchedule' d c conv tconv t r eom reg
  | otherwise
    = Left $ "isRegular size (" ++ show (length reg) ++ ") > 0 and does not match date size - 1"

-- | Make schedule from arguments (validated)
mkSchedule' ::
     [Date]                          -- ^ coupon dates
  -> Calendar                        -- ^ calendar used in date calculations
  -> BusinessDayConvention           -- ^
  -> Maybe BusinessDayConvention     -- ^ business day convention for termination dates
  -> Maybe Period                    -- ^ tenor
  -> Maybe DateGenerationRule        -- ^ how dates are to be generated in calculating dates for schedule
  -> Maybe Bool                      -- ^ End of month
  -> [Bool]                          -- ^ Boolean values for dates indicating whether regular or not
  -> Schedule
mkSchedule' d c cv tcv t r eom reg = Schedule d c cv tcv t r eom' NullDate NullDate reg
  where eom' = bool eom (Just False) $ not (isNothing t) && not (allowsEndOfMonth (fromJust t))

-- | This represents the 2nd (rule-based) constructor for Schedule.  This constructor in the QL code has more
--   conditions than the first, so is more difficult to transcribe
mkScheduleFromEffectiveDate :: 
         Settings                     -- ^ used when effective date is calculated
      -> Date                         -- ^ effective date
      -> Date                         -- ^ termination date
      -> Calendar                     -- ^ calendar dates are drawn from
      -> BusinessDayConvention        -- ^ 
      -> Maybe BusinessDayConvention  -- ^ termination date convention
      -> Maybe Period                 -- ^ tenor 
      -> Maybe DateGenerationRule     -- ^ 
      -> Maybe Bool                   -- ^ end of month
      -> Date                         -- ^ first date in schedule
      -> Date                         -- ^ next to last date
      -> Either String Schedule
mkScheduleFromEffectiveDate settings efd td c cnv tdcnv t r eom fd ntl
  = chkTermDate td 
      >> chkDateGenerationRule initsched
      >> chkEffectiveDate settings efd td ntl initsched
      >>= \e -> validEffectiveDate td e
              >> chkTenor initsched
              >>= reconcileDateGeneration e td firstDate "first date"
              >>= reconcileDateGeneration e td nextToLastDate "next to last date"
              >>= genScheduleDates e td
              >>= adjustDates
              -- >> Left "TO DO" -- TO DO!!
    where initsched = Schedule [] c cnv tdcnv t r eom' fd ntl' []
          -- initial validation of dates and calculation of initial values
          -- this is done in the constructor parameter list in the QL code
          fd'  = bool fd NullDate (efd == fd)
          ntl' = bool ntl NullDate (ntl == td) 
          eom' = bool (Just False) eom ((not . isNothing) t && (allowsEndOfMonth . fromJust) t)


-- | Check the effective date and if null and the schedule contains a Backward date
-- generation rule, AND certain conditions are satisfied, then calculate it from
-- either termination date or the evaluation date in the Settings.
chkEffectiveDate ::
     Settings
  -> Date -- current effective date to check
  -> Date -- termination date
  -> Date -- next to last date
  -> Schedule
  -> Either String Date
chkEffectiveDate s d td ntl sch
  | (fromJust . rule) sch == Backward && isNullDate d && isNullDate (firstDate sch)
    && evalDate s >= td
    = Right $ edate ntl td $ evalDate s
  | isNullDate d
    = Left "null effective date"
  | otherwise
    = Right d -- take no action
    -- Note that, at this point, we know that termination date and eval date are both not null
    -- so we can call getCalDate on them
    where edate n t e | isNullDate n = Date $ calcd (getCalDate t) (getCalDate e)
                      | otherwise    = Date $ calcd (getCalDate n) (getCalDate e)
          calcd d d' = subtractFromDate d (mkPeriodFromTime ((subtractDates d d') `div` 366 + 1) Years)

-- validation functions over schedules

-- | has a date generation rule been provided?
chkDateGenerationRule :: Schedule -> Either String Schedule
chkDateGenerationRule s | isNothing $ rule s = Left "no date generation rule provided"
                        | otherwise          = Right s

-- | has a termination date been properly specified
chkTermDate :: Date -> Either String Date
chkTermDate d | d == NullDate = Left "null termination date"
              | otherwise     = Right d

-- | is effective date < termination date
validEffectiveDate ::
     Date  -- ^ termination date
  -> Date  -- ^ effective date
  -> Either String Date
validEffectiveDate td efd
   | efd < td = Right efd
   | otherwise = Left $
                    "effective date ("
                    ++ show efd
                    ++ ") equal to or later than termination date ("
                    ++ show td
                    ++ ")" 

chkTenor :: Schedule -> Either String Schedule
chkTenor s | isNothing $ tenor s = Left "tenor undefined"
           | (==0) plen          = Right $ s{rule=Just Zero}
           | (>0) plen           = Right s
           | otherwise           = Left $ "non-positive tenor (" ++ show p ++ ") not allowed"
   where plen = lenPeriod p
         p    = (fromJust . tenor) s

-- | Reconcile date generation rule and relationships between dates
-- This is based on the case-statement over the date generation rule in the Schedule constructor.
-- The first function checks that the date to be reconciled is not null, since the reconciliation
-- process only applies to non-null dates.
reconcileDateGeneration ::
     Date                -- ^ effective date
  -> Date                -- ^ termination date
  -> (Schedule -> Date)  -- ^ function to extract date from schedule to compare against effective and term dates
  -> String              -- ^ description of date to be compared
  -> Schedule
  -> Either String Schedule
reconcileDateGeneration e t f s sch
  = bool (reconcileDateGeneration' (getCalDate e) (getCalDate t) f s sch) (Right sch) (isNullDate $ f sch)

reconcileDateGeneration' ::
     CalDate               
  -> CalDate
  -> (Schedule -> Date)
  -> String             
  -> Schedule
  -> Either String Schedule
reconcileDateGeneration' e td fd sd s
  | elem r [Backward,Forward] = bool (Left $ err_pref ++ err_suff1) (Right s) (d > e && d < td)
  | r == ThirdWednesday       = bool (Left $ err_pref ++ err_suff2) (Right s) (isIMMdate d False)
    -- the following date generation rules require a null date
  | elem r [Zero,Twentieth,TwentiethIMM,OldCDS,CDS,CDS2015]
                              = bool (Left err_incompat) (Right s) False
  | otherwise                 = Left "reconcileDateGeneration::unknown date generation rule"
  where r = fromJust . rule $ s
        d = getCalDate $ fd s
        err_pref = sd ++ "(" ++ show d ++ ") "
        err_suff1 = "out of effective-termination date range"
        err_suff2 = "is not an IMM date"
        err_incompat = sd ++ " incompatible with " ++ show r ++ " date generation rule"

-- | Determine how schedule dates should be generated based on the date generation rule
-- We assume that tenor has been checked by this point and therefore can extract valid period

genScheduleDates ::
     Date                    -- ^ Effective date
  -> Date                    -- ^ Termination date
  -> Schedule
  -> Either String Schedule
genScheduleDates ed td sch 
  | rule sch == Just Zero      = scheduleDatesZero ed td sch
  | rule sch == Just Backward  = scheduleDatesBackward ed td sch
  | rule sch == Just Forward   = undefined
  | elem (fromJust . rule $ sch) [Twentieth,TwentiethIMM,OldCDS,CDS,CDS2015]
    = bool
        (Left $ "endofmonth convention incompatible with " ++ (show $ rule sch) ++ "date generation rule")
        (undefined)
        (not . isNothing $ useEndOfMonth sch)
  | otherwise
    = Left "scheduleDates::unknown date generation rule"

-- | Schedule date calculation where date generation rule is Zero
scheduleDatesZero ::
     Date
  -> Date
  -> Schedule
  -> Either String Schedule
scheduleDatesZero ed td sch
  = Right $ sch{tenor=Just zyrs, dates = dl, regular = rs}
    where zyrs = mkPeriodFromTime 0 Years
          dl = dates sch ++ [ed,td]
          rs = regular sch ++ [True]

-- | Schedule date calculation where date generation rule is Backward
scheduleDatesBackward ::
     Date         -- ^ effective date
  -> Date         -- ^ termination date
  -> Schedule
  -> Either String Schedule
scheduleDatesBackward ed td sch
   = Right $ Schedule {dates = d} -- TO DO, the start/end conditions from QL code, plus dealing with dups after adjustment.
   where d = (reverse . prefixDates . fmap Date . takeWhile (>= exitDate)) $
                datesList
                  nullCalendar
                  (getCalDate seedDate)
                  (negate $ lenPeriod t)
                  (units t)
                  (convention sch)
                  (fromJust $ useEndOfMonth sch)
         t = fromJust $ tenor sch
         tdc = getCalDate td
         ntl = nextToLastDate sch
         fd  = firstDate sch
         seedDate | (not . isNullDate) ntl = ntl
                  | otherwise              = td
         exitDate | (not . isNullDate) fd = getCalDate fd
                  | otherwise             = getCalDate ed
         prefixDates ds | (not . isNullDate) ntl = td : ds
                        | otherwise              = ds

-- | Create a list of dates for a schedule
datesList ::
     Calendar    -- ^ Calendar to use in calculating next date
  -> CalDate     -- ^ Date to start from.  Use CalDate because doesn't make sense to have NullDates
  -> Int         -- ^ how much to advance each date by (used in conjunction with TimeUnit)
  -> TimeUnit    -- ^ units of advancement
  -> BusinessDayConvention -- ^ how dates should be adjusted if holiday
  -> Bool        -- ^ should end of months be catered for if TimeUnit is Months or Years
  -> [CalDate]
datesList cal d t tu bc eom
  = d : datesList cal (advanceDateByUnit cal d t tu bc eom) t tu bc eom

-- | apply adjustments to the generated schedule date
-- TO DO - add other adjustments
adjustDates :: Schedule -> Either String Schedule
adjustDates = Right . adjust3rdWednesday

adjust3rdWednesday :: Schedule -> Schedule
adjust3rdWednesday s
  | (fromJust . rule) s == ThirdWednesday = s { dates = d }
  | otherwise                             = s 
    where d = ((:) . head) <*> (catMaybes . map (f . getCalDate) . tail) $ dates s
          f sd = nthWeekDay 3 Wednesday (getmonth sd) (getyear sd)  

adjustEOM :: Schedule -> Schedule
adjustEOM s
  | convention s == Unadjusted = undefined
  | otherwise                  = undefined
-- Misc. schedule functions

-- | Length of dates list - equivalent to size() function in QL Schedule class
schLength :: Schedule -> Int
schLength = length . dates

-- | extract a particular date given its position in the schedule's list of coupon dates
at :: Schedule -> Int -> Maybe Date
at s i | i < length ds = Just $ ds !! i
       | otherwise     = Nothing
   where ds = dates s


-- | for a given date in the schedule, get the next date
-- In the QL code, if there is no next date, then a null date is returned.  Also, if the date
-- is the same as a date in the Schedule's list of dates, then we return the same date (this
-- mirrors what QL does using lower_bound).
nextDate :: Schedule -> Date -> Date
nextDate = undefined

-- | for a given date in the schedule, get the previous date
-- Similarly to nextDate, if there is no previous date, then return a null date
prevDate :: Schedule -> Date -> Date
prevDate = undefined

-- | is a date, as indicated by its positionin the schedule, regular?  We assume that
-- the position argument starts at 1 (as per QL)
isRegular :: Schedule -> Int -> Maybe Bool
isRegular s i | i > 0 && i <= (length . dates) s = Just $ regular s !! (i - 1)
              | otherwise                        = Nothing

isEmpty :: Schedule -> Bool
isEmpty = (==) 0 . length . dates





