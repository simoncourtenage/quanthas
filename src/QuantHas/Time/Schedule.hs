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
					tenor :: Maybe Period,
					calendar :: Calendar,
					convention :: BusinessDayConvention,
					terminationDateConvention:: Maybe BusinessDayConvention,
					rule :: Maybe DateGenerationRule,
					endOfMonth :: Maybe Bool,
					firstDate :: Date,
					nextToLastDate :: Date,
					dates :: Array Date,
					isRegular :: Array Bool
				}

makeSchedule :: Array Date -> Calendar -> BusinessDayConvention -> Maybe BusinessDayConvention -> Maybe Period -> 
					Maybe DateGenerationRule -> Maybe Bool -> Date -> Date -> Schedule
makeSchedule dates cal convention tdConvention tenor rule endOfMonth isRegular
	= Schedule dates cal convention tdConvention tenor rule endOfMonth isRegular

