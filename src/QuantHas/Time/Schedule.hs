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
import QuantHas.Time.Calendars.NullCalendar
import QuantHas.Time.BusinessDayConvention
import QuantHas.Time.Period
import QuantHas.Settings
import QuantHas.Require

{-
    Schedule are used to represent sequences of coupon dates.
-}

data Schedule = Schedule
				{
					dates :: Array Date,
					calendar :: Calendar,
					convention :: BusinessDayConvention,
					terminationDateConvention:: Maybe BusinessDayConvention,
					tenor :: Maybe Period,
					rule :: Maybe DateGenerationRule,
					endOfMonth :: Maybe Bool,
					firstDate :: Date,
					nextToLastDate :: Date,
					isRegular :: Array Bool
				}

mkSchedule :: Array Date -> Calendar -> BusinessDayConvention -> Maybe BusinessDayConvention -> Maybe Period -> 
					Maybe DateGenerationRule -> Maybe Bool -> Date -> Date -> Schedule
mkSchedule dates cal convention tdConvention tenor rule endOfMonth isRegular
	= Schedule dates cal convention tdConvention tenor rule endOfMonth_ mkNullDate mkNullDate isRegular
	  where
	  endOfMonth_ = if not (isNothing tenor) and not allowsEndOfMonth tenor then False else endOfMonth

-- this represents the 2nd (rule-based) constructor for Schedule.  This constructor in the QL code has more
-- conditions than the first, so is more difficult to transcribe
mkScheduleFromEffectiveDate
   :: Settings -> Date -> Date -> Calendar -> BusinessDayConvention -> Maybe BusinessDayConvention -> Period -> Maybe DateGenerationRule
      -> Maybe Bool -> Date -> Date -> Schedule
mkScheduleFromEffectiveDate _ _ (Date _ _ _ 0) _ _ _ _ _ _ _ _  = error "Null termination date"
mkScheduleFromEffectiveDate _ (Date _ _ _ 0) termDate cal con tdcon ten Backward eom (Date _ _ _ 0) nextToLastDate
	= error "Not complete" -- needs effective date to be retrieved from settings monad?
-- the constructor in Quantlib that corresponds to the next function is complex - we split the different
-- functionalities of the C++ constructor across several helper functions
mkScheduleFromEffectiveDate settings effDate termDate cal convention tdConvention tenor rule endOfMonth firstDate nextToLastDate
   = let
   	 	effectiveDate = checkEffectiveDate (calcEffectiveDate settings termDate firstDate nextToLastDate rule) termDate
   	 	rule'         = if (lenPeriod tenor == 0) then
   	 						       Zero
   	 					        else
                        if (lenPeriod tenor < 0) then
   	 						           require (not (lenPeriod tenor < 0)) rule "Non postive tenor not allowed"
                        else
                           error "mkScheduleFromEffectiveDate: bad tenor"
   	 	firstDate_	  = if (firstDate == effectiveDate) then mkNullDate else firstDate
   	 	ntlDate_	  = if (nextToLastDate == termDate) then mkNullDate else nextToLastDate
   	 	firstDate'	  = chkDateAgainstRule firstDate_ rule' "First date"
   	 	ntlDate'	  = chkDateAgainstRule nextToLastDate_ rule' "Next to last date"
   	 	endOfMonth' 		  = if allowsEndOfMonth tenor then endOfMonth else Just False
   	 	schedule 	  = Schedule [] cal convention tdConvention tenor rule' endOfMonth' firstDate' ntlDate' []
   	 	schedule'	  = calcSchedule schedule nullCalendar effectiveDate termDate firstDate
   	 in
   		schedule'

{-
	What the constructor does - in order
	1. decides if effective date needs to be calculated, and if yes, calculates it
	2. checks if rule needs to be set
	3. sanity checks rule against date
	4. calculates dates using rule etc.  - really complex!
-}

-- used to create Schedule
calcEffectiveDate :: Settings -> Date -> Date -> Date -> Date -> DateGenerationRule -> Date
calcEffectiveDate settings termDate (Date 0 0 0 0) (Date 0 0 0 0) nextToLast Backward
   = if (isNullDate nextToLast) then
   			termDate - ((termDate - evalDate)/366+1)*Years
   	 else
   	 		nextToLast - ((nextToLast - evalDate)/366+1)*Years
   	 where
   	 evalDate = evaluationDate settings
calcEffectiveDate _ _ effdate _ _ _ = require (not (isNullDate effdate)) "Null effective date" effdate

checkEffectiveDate :: Date -> Date -> Date
checkEffectiveDate effDate termDate
   = require (effDate < termDate) ("Effective date " + effDate + " equal to or later than termination date") effDate

{-
	chkDateAgainstRule is used to check first date and next to last date against the dategeneration rule.
	In the C++ code for the Schedule constructor, this is represented by two switch statements - which do
	the same thing over the two dates.  Here we return the date unaffected so long as the checks are passed
	or throw an error otherwise (using require)
-}

chkDateAgainstRule :: Date -> Date -> Date -> Rule -> String -> Date
chkDateAgainstRule d@(Date _ _ _ 0) _ _ r _ = d
chkDateAgainstRule fd ed td r datename
	= if (elem r [Backward,Forward]) then
		require (fd > ef && fd < td) (datename + " out of range effective date - termination date") fd
	  else if (r == ThirdWednesday) then
		require (False) (datename + " (" + fd + ") is not an IMM date") fd
	  else if (elem r [Zero,Twentieth,TwentiethIMM,OldCDS,CDS]) then
		error datename + " (" + fd + ") incompatible with date generation rule " + r
	  else
		error "Unknown rule " + r

{-
	calcSchedule is the penultimate stage of the Schedule constructure in Quantlib, which calculates the dates based
	on the DateGeneration rule.  The C++ code uses a switch statement, which here is represented using pattern matching.
-}

calcSchedule :: Schedule -> Calendar -> Date -> Date -> Date -> Schedule
calcSchedule (Schedule d cal conv tdconv tenor Zero eom fdate ntldate isreg) ncal edate tdate fstdate
	= Schedule newdates cal conv tdconv tenor' r@Zero eom fdate ntldate (isreg ++ [true])
	  where
	  newdates = d ++ [edate,tdate]
	  tenor' = makePeriodFromTime 0 Years
calcSchedule sch@(Schedule ds cal conv tdconv tenor r@Backward eom fdate ntldate isreg) ncal edate tdate fstdate
	= calcScheduleBackward (Schedule ds' cal conv tdconv tenor r eom fdate ntldate isreg) ncal edate tdate (seed,exitd)
	  where
    {- If we have a nextToLast date, then use it and calculate the regularity of the interval
       calcIsReg checks that the nextToLast date is a whole Period behind the termination date.
       If it is, then the interval between them is regular, else it is not.
    -}
	  (ds',isreg') = let calcIsReg = isIntervalRegular conv eom ntldate tdate (-1*tenor)
                   in
                      if (not (isNullDate ntldate)) then
                        ([ntldate,tdate],[calcIsReg])
                      else
                        ([tdate],[])
	  seed = tdate
	  exitd = if not isNullDate fstdate then
              fstdate
            else
              edate


-- | Calculate the coupon dates backwards from the termination date.
calcScheduleBackward :: Schedule
                        -> Calendar
                        -> Date         -- ^ effective date for start of schedule of payments
                        -> Date         -- ^ termination date for schedule of payments
                        -> (Date,Date)  -- ^ seed date and exit date
                        -> Schedule
calcScheduleBackward (Schedule ds cal conv tdconv tenor r eom fdate ntldate isreg) ncal edate tdate (seedd,exitd)
	= Schedule coupondates cal conv tdconv tenor r eom fdate ntldate regs
    where
      (coupondates,regs) | isNullDate ntldate = schDatesBackward exitdate fdate tdate ([tdate],[])
                         | otherwise          = let
                                                  isreg' = if (advanceDateByPeriod tdate (makePeriodFromTime 0 tenor bdc eom)) /= ntldate
                                                  then
                                                    False
                                                  else
                                                    True
                                                in
                                                schDatesBackward exitdate fdate ntldate ([tdate,ntldate],[isreg'])
      exitdate  | isNullDate fdate   = edate -- if first date is null, then exit date is effective date
                | otherwise          = fdate

-- | calculates dates and interval regularity for schedule backwards
schDatesBackward :: Date
                    -> Date             -- ^ exit date from schedule
                    -> Date             -- ^ first date of schedule
                    -> Int              -- ^ number of periods seen so far
                    -> ([Date],[Bool])  -- ^ accumulative list of coupon dates and interval flags
                    -> ([Date],[Bool])  -- ^ list of coupon dates and interval flags for schedule
schDatesBackward exitdate firstdate lastdate (dates,intervals)
    = if nextdate < exitdate
      then  (dates',intervals')
      else (dates,intervals)
      where
      dates' | (not $ isNullDate firstdate) && 



-- | Used to determine whether an interval between two dates is regular.  If the distance from date2 by units is
-- |equal to date1, then interval between date1 and date2 is regular.
isIntervalRegular :: BusinessDateConvention
                     -> Bool    -- ^ end of month flag
                     -> Date    -- ^ date marking start of interval
                     -> Date    -- ^ date marking end of interval
                     -> Int     -- ^ length of interval
                     -> Bool    -- ^ True if interval between dates is regular
isIntervalRegular conv eom date1 date2 units | date1 == advanceDateByPeriod date2 units conv eom = True
                                             | otherwise                                         = False
