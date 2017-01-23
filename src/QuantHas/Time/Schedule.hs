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
import QuantHas.Time.BusinessDayConvention

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
   :: Date -> Date -> Calendar -> BusinessDayConvention -> Maybe BusinessDayConvention -> Period -> Maybe DateGenerationRule
      -> Maybe Bool -> Date -> Date -> Schedule
mkScheduleFromEffectiveDate _ (Date _ _ _ 0) _ _ _ _ _ _ _ _  = error "Null termination date"
mkScheduleFromEffectiveDate (Date _ _ _ 0) termDate cal con tdcon ten Backward eom (Date _ _ _ 0) nextToLastDate
	= error "Not complete" -- needs effective date to be retrieved from settings monad?

calcEffectiveDate :: Date -> Date -> Date -> DateGenerationRule -> Date
calcEffectiveDate (Date 0 0 0 0) (Date 0 0 0 0) nextToLast Backward
   | isNullDate nextToLast = 

mkScheduleFromEffectiveDate effDate termDate cal convention tdConvention tenor rule endOfMonth firstDate nextToLastDate
   = Schedule dates cal convention tdConvention tenor rule endOfMonth firstDate nextToLastDate isRegular
     where -- this is where the fun starts







