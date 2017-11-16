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
  Status: work in progress - will not compile
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
              >>= scheduleDates e td
              >> Left "TO DO"
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

scheduleDates ::
     Date                    -- ^ Effective date
  -> Date                    -- ^ Termination date
  -> Schedule
  -> Either String Schedule
scheduleDates ed td sch 
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
-- In the QL code, the dates list is effectively built backwards by using a negative value for timeunit and inserting
-- dates at the front of the list.  We could do that, but you get the same effect by building it forwards using a positive
-- value for time unit length, and, like that, we can have a general purpose method for generating a dates list and save
-- reversing the list for backwards.
-- TO DO - special start/end conditions from QL code, dealing with dups on adjustment and generation of regular interval booleans
scheduleDatesBackward ::
     Date         -- ^ effective date
  -> Date         -- ^ termination date
  -> Schedule
  -> Either String Schedule
scheduleDatesBackward ed td sch
   = Right $ Schedule {dates = d} -- TO DO, the start/end conditions from QL code, plus dealing with dups after adjustment.
   where d = (fmap Date . takeWhile (<= tdc)) $
                datesList nullCalendar edc (lenPeriod t) (units t) (convention sch) (fromJust $ useEndOfMonth sch)
         t = fromJust $ tenor sch
         tdc = getCalDate td
         edc = getCalDate ed

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

-- the constructor in Quantlib that corresponds to the next function is complex - we split the different
-- functionalities of the C++ constructor across several helper functions

{--

-- commented out this block - being reworked

mkScheduleFromEffectiveDate
  settings effDate termDate cal convention tdConvention tenor (Just rule) endOfMonth firstDate nextToLastDate
   = let
   	 	effectiveDate = chkEffectiveDate (calcEffectiveDate settings termDate effDate firstDate nextToLastDate rule) termDate
   	 	rule'         = if (lenPeriod (fromJust tenor) == 0) then
   	 						       Zero
   	 					        else
                        undefined
                      -- TO DO
   	 						       --require (not (lenPeriod (fromJust tenor) < 0)) "Non positive tenor not allowed" rule
   	 	firstDate_	  = if firstDate == effectiveDate then mkNullDate else firstDate
   	 	ntlDate_	    = if nextToLastDate == termDate then mkNullDate else nextToLastDate
   	 	endOfMonth'   = if isJust tenor && allowsEndOfMonth (fromJust tenor) then endOfMonth else Just False
   	 	schedule 	    = Schedule [] cal convention tdConvention tenor rule' endOfMonth' firstDate_ ntlDate_ []
     in
   	 	calcSchedule nullCalendar effectiveDate termDate firstDate (chkScheduleDates schedule effDate termDate)

{--
	What the QL version of schedule constructor does - in order
	1. decides if effective date needs to be calculated, and if yes, calculates it
	2. checks if rule needs to be set
	3. sanity checks rule against date
	4. calculates dates using rule etc.  - really complex!
--}

calcEffectiveDate :: Settings               -- ^ QuantHas settings
                     -> Date                -- ^ termination date
                     -> Date                -- ^ effective date as supplied
                     -> Date                -- ^ first date supplied for schedule
                     -> Date                -- ^ next to last date
                     -> DateGenerationRule  -- ^ how should we generate dates for the schedule?
                     -> Either String Date
-- effective date is null and schedule is backward
calcEffectiveDate settings termDate (Date 0 0 0 0) (Date 0 0 0 0) ntldate Backward
  | isNullDate ntldate = Left $ subtractFromDate termDate (mkPeriodFromTime ((subtractDates termDate evalDate)/366+1) Years)
  | otherwise          = Left $ subtractFromDate ntldate (mkPeriodFromTime ((subtractDates ntldate evalDate)/366+1) Years)
   	where
   	evalDate = evaluationDate settings
calcEffectiveDate _ _ effdate _ _ _  | isNullDate effdate = Left "Null effective date"
                                     | otherwise          = Right effdate

-- | sanity check that effective date is not equal to or later than termination date
--   TO DO: is this true even if BACKWARDS?
chkEffectiveDate :: Date -> Date -> Either String Date
chkEffectiveDate effDate termDate
   | effDate < termDate = Right effDate
   | otherwise          = Left  $ "Effective date " + show effDate + " equal to or later than termination date"

{--
  chkScheduleDates is used to check first date and next to last date against the dategeneration rule.
	In the C++ code for the Schedule constructor, this is represented by two switch statements - which do
	the same thing over the two dates.
--}
chkScheduleDates :: Schedule
                    -> Date       -- ^ effective date
                    -> Date       -- ^ termindation date
                    -> Either String Schedule
chkScheduleDates s@(Schedule _ _ _ _ _ Backward _ fdate ntldate _) edate tdate
  | fdate > edate && fdate < tdate = Right s
  | otherwise = Left $ show fdate + " out of range effective date - termination date"
chkScheduleDates s@(Schedule _ _ _ _ _ Forward _ fdate ntldate _) edate tdate
  | fdate > edate && fdate < tdate = Right s
  | otherwise = Left (show fdate + " out of range effective date - termination date")


{-
	calcSchedule is the penultimate stage of the Schedule constructure in Quantlib, which calculates the dates based
	on the DateGeneration rule.  The C++ code uses a switch statement, which here is represented using pattern matching.
-}

calcSchedule :: Calendar -> Date -> Date -> Date -> Schedule -> Schedule
calcSchedule ncal edate tdate fstdate (Schedule d cal conv tdconv tenor Zero eom fdate ntldate isreg) 
	= Schedule newdates cal conv tdconv (Just tenor') Zero eom fdate ntldate (isreg ++ [True])
	  where
	  newdates = d ++ [edate,tdate]
	  tenor' = mkPeriodFromTime 0 Years
calcSchedule ncal edate tdate fstdate sch@(Schedule ds cal conv tdconv tenor r@Backward eom fdate ntldate isreg) 
	= calcScheduleBackward (Schedule ds' cal conv tdconv tenor r eom fdate ntldate isreg')
        ncal edate tdate (seed,exitd) (nextSchDate nullCalendar (fromJust tenor) conv (fromJust eom))
	  where
       {-
        If we have a nextToLast date, then use it and calculate the regularity of the interval
        calcIsReg checks that the nextToLast date is a whole Period behind the termination date.
        If it is, then the interval between them is regular, else it is not.
       -}
	     (ds',isreg') = let calcIsReg = isIntervalRegular conv (fromJust eom) ntldate tdate (-1*tenor)
                      in
                          if (not (isNullDate ntldate)) then
                            ([ntldate,tdate],[calcIsReg])
                          else
                            ([tdate],[])
	     seed      = if isNullDate ntldate then tdate else ntldate
	     exitd     = if isNullDate fstdate then edate else fstdate


{-
  Calculate coupon dates backwards from termination date.
-}
calcScheduleBackward :: Schedule
                        -> Calendar
                        -> Date           -- ^ effective date for start of schedule of payments
                        -> Date           -- ^ termination date for schedule of payments
                        -> (Date,Date)    -- ^ seed date and exit date
                        -> (Date -> Int -> Date) -- ^ function for calculating next date in sequence
                        -> Schedule
calcScheduleBackward (Schedule ds cal conv tdconv tenor r eom fdate ntldate isreg) ncal edate tdate (seedd,exitd) ndf
	= Schedule coupondates cal conv tdconv tenor r eom fdate ntldate regs
    where
      (coupondates,regs) = schDatesBackward cal exitd fdate seedd tenor conv eom 1 (ds,isreg) ndf (ndf seed (-1))

{-
  Calculates dates and interval regularity for schedule backwards
-}
schDatesBackward :: Calendar
                    -> Date             -- ^ exit date from schedule
                    -> Date             -- ^ first date of schedule
                    -> Date             -- ^ seed date
                    -> Period           -- ^ 'tenor' of schedule
                    -> BusinessDayConvention
                    -> Bool             -- ^ end of month?
                    -> Int              -- ^ number of periods seen so far
                    -> ([Date],[Bool])  -- ^ accumulative list of coupon dates and interval flags
                    -> (Date -> Int -> Date) -- ^ function for calculating next date in schedule
                    -> ([Date],[Bool])  -- ^ list of coupon dates and interval flags for schedule
schDatesBackward cal exitdate firstdate seeddate tenor conv eom periods (dates,intervals) ndf nextdate
    | nextdate < exitdate = if ((not $ isNullDate firstdate) && adjustCalendarDate cal hddate conv != adjustCalendarDate cal firstdate conv)
                            then
                              (firstdate : dates, False : intervals)
                            else
                              (dates,intervals)
    | otherwise           = if (adjustCalendarDate cal hddate conv != adjustCalendarDate cal nextdate conv)
                            then
                              schDatesBackward cal exitdate firstdate seeddate tenor conv eom (periods+1) (nextdate: dates,True:intervals) ndf ndate'
                            else
                              schDatesBackward cal exitdate firstdate seeddate tenor conv eom (periods+1) (dates,intervals) ndf ndate'
                            where
                            hddate = head dates
                            ndate' = ndf seed (negate $ periods+1)


-- | Calculate next date in schedule
nextSchDate :: Calendar                   -- ^ Calendar to use when advancing date
               -> Period                  -- ^ Specification of interval in form of Period value
               -> BusinessDayConvention  -- ^ business day convention
               -> Bool                    -- ^ use end of month?
               -> Date                    -- ^ advance from this 'seed' date
               -> Int                     -- ^ how many intervals to be counted from seed date to new date
               -> Date
nextSchDate cal p conv eom seed intcnt
  = advanceDateByPeriod cal seed (periodOp (*intcnt) p) conv eom -- ???


-- | Used to determine whether an interval between two dates is regular.  If the distance from date2 by units is
-- |equal to date1, then interval between date1 and date2 is regular.
isIntervalRegular :: BusinessDayConvention
                     -> Bool    -- ^ end of month flag
                     -> Date    -- ^ date marking start of interval
                     -> Date    -- ^ date marking end of interval
                     -> Int     -- ^ length of interval
                     -> Bool    -- ^ True if interval between dates is regular
isIntervalRegular conv eom date1 date2 units | date1 == advanceDateByPeriod date2 units conv eom = True
                                             | otherwise                                         = False

--}

-- Schedule functions

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





