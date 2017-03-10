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

module QuantHas.Time.Period (module QuantHas.Time.Frequency,module QuantHas.Time.Period) where

import QuantHas.Time.Frequency
import QuantHas.Time.TimeUnit

-- unlike quantlib, we store the frequency in the period object to save recalculating it later
data Period = Period { periodLength :: Int, periodUnits :: TimeUnit, periodFrequency :: Frequency }
              deriving (Eq, Show)

instance Ord Period where
    (<) p1 p2 = lessThan p1 p2
    (>=) p1 p2 = not (lessThan p1 p2)
    (<=) p1 p2 = lessThanOrEqual p1 p2

-- use this as a wrapper round the different values that can be used to create a period value
data PeriodArgs = PeriodTimeArgs Int TimeUnit | PeriodFreqArg Frequency

-- unwrap the arguments and use the data constructor to switch to the appropriate constructor
mkPeriod :: PeriodArgs -> Period
mkPeriod (PeriodTimeArgs len tunit) = mkPeriodFromTime len tunit
mkPeriod (PeriodFreqArg freq) = mkPeriodFromFrequency freq

mkPeriodFromTime :: Int -> TimeUnit -> Period
mkPeriodFromTime n tunit = Period n tunit (calcFrequency (abs n) tunit)

mkPeriodFromFrequency :: Frequency -> Period
mkPeriodFromFrequency p@NoFrequency = Period 0 Days p
mkPeriodFromFrequency p@Once        = Period 0 Years p
mkPeriodFromFrequency p@Annual      = Period 1 Years p
mkPeriodFromFrequency p@Semiannual  = Period (12 `div` (lookupFrequency p)) Months p
mkPeriodFromFrequency p@EveryFourthMonth = Period (12 `div` (lookupFrequency p)) Months p
mkPeriodFromFrequency p@Quarterly   = Period (12 `div` (lookupFrequency p)) Months p
mkPeriodFromFrequency p@Bimonthly   = Period (12 `div` (lookupFrequency p)) Months p
mkPeriodFromFrequency p@Monthly     = Period (12 `div` (lookupFrequency p)) Months p
mkPeriodFromFrequency p@EveryFourthWeek = Period (52 `div` (lookupFrequency p)) Weeks p
mkPeriodFromFrequency p@Biweekly    = Period (52 `div` (lookupFrequency p)) Weeks p
mkPeriodFromFrequency p@Weekly      = Period (52 `div` (lookupFrequency p)) Weeks p
mkPeriodFromFrequency p@Daily       = Period 1 Days p
mkPeriodFromFrequency _             = error "mkPeriodFromFrequency - OtherFrequency specified"

-- normalize function

normalize :: Period -> Period
normalize p@(Period 0 _ _ )       = p
normalize p@(Period len Days freq) | len `mod` 7 == 0 = Period newlen Weeks (calcFrequency newlen Weeks)
                                   | otherwise        = p
                                                        where newlen = len `div` 7
normalize p@(Period len Months freq) | len `mod` 12 == 0 = Period newlen Years (calcFrequency newlen Years)
                                   | otherwise           = p
                                                           where newlen = len `div` 12

units :: Period -> TimeUnit
units (Period _ tu _) = tu

lenPeriod:: Period -> Int
lenPeriod (Period l _ _) = l

-- calculate minimum and maximum possible number of days for a period
daysMinMax :: Period -> (Int,Int)
daysMinMax (Period l Days _)   = (l,l)
daysMinMax (Period l Weeks _)  = (l*7,l*7)
daysMinMax (Period l Months _) = (28*l,31*l)
daysMinMax (Period l Years _)  = (365*l,366*l)

-- operators over Periods

lessThanOrEqual :: Period -> Period -> Bool
lessThanOrEqual p1 p2 = lessThan p1 p2 || equalPeriod p1 p2

equalPeriod :: Period -> Period -> Bool
equalPeriod p1 p2
   = periodFrequency p1 == periodFrequency p2 && periodUnits p1 == periodUnits p2 && periodFrequency p1 == periodFrequency p2

lessThan :: Period -> Period -> Bool
lessThan (Period 0 u1 f1) (Period l2 u2 f2) = l2 > 0
lessThan (Period l1 u1 f1) (Period 0 u2 f2) = l1 < 0
lessThan (Period l1 Months f1) (Period l2 Years f2) = l1 < l2*12
lessThan (Period l1 Years f1) (Period l2 Months f2) = l1*12 < l2
lessThan (Period l1 Days f1) (Period l2 Weeks f2) = l1 < l2*7
lessThan (Period l1 Weeks f1) (Period l2 Days f2) = l1*7 < l2
lessThan p1 p2 = if (units p1 == units p2) then (lenPeriod p1 < lenPeriod p2)
                 else
                      let (min1,max1) = daysMinMax(p1)
                          (min2,max2) = daysMinMax(p2)
                      in
                      if max1 < min2 then True
                      else if (min1 > max2) then False
                           else error "Undecidable period comparison" 

-- this is in time/schedule.hpp but is placed here because it is a function over Period values
allowsEndOfMonth :: Period -> Bool                                                         
allowsEndOfMonth p@(Period _ tu _)
   = (tu == Months || tu == Years) && (p >= (Period 1 Months (calcFrequency (abs 1) Months)))



