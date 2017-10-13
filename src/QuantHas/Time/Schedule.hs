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

module QuantHas.Time.Schedule(module QuantHas.Time.Schedule) where

import Data.Array
import Data.Maybe
import QuantHas.Time.Date
import QuantHas.Time.Calendar.NullCalendar
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.DateGeneration
import QuantHas.Time.Period
import QuantHas.Settings

{-
    Schedule are used to represent sequences of coupon dates from a calendar
-}

data Schedule = Schedule
				{
					dates :: [Date],
					calendar :: Calendar,
					convention :: BusinessDayConvention,
					terminationDateConvention:: Maybe BusinessDayConvention,
					tenor :: Maybe Period,
					rule :: DateGenerationRule,
					endOfMonth :: Maybe Bool,
					firstDate :: Date,
					nextToLastDate :: Date,
					isRegular :: [Bool]
				}

mkSchedule
  :: [Date]                          -- ^ coupon dates
  -> Calendar                        -- ^ calendar dates taken from
  -> BusinessDayConvention           -- ^
  -> Maybe BusinessDayConvention     -- ^
  -> Maybe Period                    -- ^ tenor
  -> Maybe DateGenerationRule        -- ^
  -> Maybe Bool                      -- ^ End of month
  -> [Bool]                          -- ^
  -> Either String Schedule
mkSchedule _ _ _ _ _ Nothing _ _
  = Left "No date generation rule provided"
mkSchedule ds c co tco t (Just r) eom reg
	= Right $ Schedule ds c co tco t r eom' mkNullDate mkNullDate reg
	  where
	  eom' = if not (isNothing t) && not (allowsEndOfMonth (fromJust t)) then Just False else eom

-- | This represents the 2nd (rule-based) constructor for Schedule.  This constructor in the QL code has more
--   conditions than the first, so is more difficult to transcribe
mkScheduleFromEffectiveDate
   :: Settings                        -- ^ used when effective date is calculated
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

{--      
mkScheduleFromEffectiveDate _ _ (Date _ _ _ 0) _ _ _ _ _ _ _ _  = Left "Null termination date"
mkScheduleFromEffectiveDate _ _ _ _ _ _ _ Nothing _ _ _         = Left "No date generation rule provided"
mkScheduleFromEffectiveDate _ (Date _ _ _ 0) td c con tdcon t (Just Backward) eom (Date _ _ _ 0) ntl
	= Left "Not complete" -- needs effective date to be retrieved from settings?
--}

mkScheduleFromEffectiveDate s efd td c cnv tdcnv t r eom fd ntl
  = chkNullDate td
    >>= chkDateGenRule
    >>= Left "Got here"

isTermDateNull :: Date -> Either String Date
isTermDateNull 

testMkSch  = mkScheduleFromEffectiveDate undefined undefined mkNullDate undefined undefined undefined undefined undefined undefined undefined undefined

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
