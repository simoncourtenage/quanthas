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
  Description: Base Calendar Implementation data declaration
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
-}

module QuantHas.Time.Calendar.CalendarImpl where

import Data.Array
import QuantHas.Time.DayName

type Year = Int

-- | base for lookup calculations in easter monday lists
baseYear = 1900 :: Year

data CalImpl
   = CalImpl
      {
        -- | takes the number of the day in the year, the year
        --   and returns whether or not that day is Easter Monday
        ciIsEasterMonday :: Int -> Year -> Bool,
        -- | returns day of the year Easter Monday falls on
        ciGetEasterMonday :: Year -> Int,
        ciIsWeekend :: DayName -> Bool,
        ciEasterMondays :: Array Int Int
      }

-- | checks to see if the specified day and year correspond to an easter monday in the array of easter 
checkIsEasterMonday :: CalImpl -> Int -> Year -> Bool
checkIsEasterMonday c d y = d == (ciEasterMondays c ! (y - baseYear))

-- | get day of the year easter monday falls on, given a particular calendar of easter mondays
lookupEasterMonday :: CalImpl -> Year -> Int
lookupEasterMonday c y = ciEasterMondays c ! (y - baseYear)

convEasterMondays :: [Int] -> Array Int Int
convEasterMondays e = array (1,length e) (zip [1..] e)