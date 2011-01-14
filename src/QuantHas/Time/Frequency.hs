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

-- | The module contains the Frequency data type identical to QuantLib and
-- related functions.
--
module QuantHas.Time.Frequency (
        Frequency (..),
        lookupFrequency,
        calcFrequency
    ) where

import QuantHas.Time.TimeUnit

-- | The Frequency data type used to represent common units of
-- frequency.
data Frequency
    = NoFrequency -- ^ is \"0 'TimeUnit'\" where 'TimeUnit' is NOT 'Years'
    | Once -- ^ is \"0 'Years'\", same as Quantlib.
    | Annual
    | Semiannual
    | EveryFourthMonth
    | Quarterly
    | Bimonthly
    | Monthly
    | EveryFourthWeek
    | Biweekly
    | Weekly
    | Daily
    | OtherFrequency
    deriving (Eq,Show)


-- | Return the number of repetitions in a year given a 'Frequency'.
-- For example, given 'Monthly', it will return 12.
lookupFrequency :: Frequency -> Int
lookupFrequency freq =
    case freq of
        NoFrequency      -> (-1)
        Once             -> 0
        Annual           -> 1
        Semiannual       -> 2
        EveryFourthMonth -> 3
        Quarterly        -> 4
        Bimonthly        -> 6
        Monthly          -> 12
        EveryFourthWeek  -> 13
        Biweekly         -> 26
        Weekly           -> 52
        Daily            -> 365
        OtherFrequency   -> 999

-- | Finds the corresponding 'Frequency' for the given time interval.
-- The time interval is given in terms of an Int and a TimeUnit.
-- If not a known frequency, 'OtherFrequency' is returned. Note the difference
-- between 'Once' and 'NoFrequency'
calcFrequency :: Int -> TimeUnit -> Frequency
calcFrequency 0 Years  = Once
calcFrequency 0 _      = NoFrequency
calcFrequency 1 Years  = Annual
calcFrequency 6 Months = Semiannual
calcFrequency 4 Months = EveryFourthMonth
calcFrequency 3 Months = Quarterly
calcFrequency 2 Months = Bimonthly
calcFrequency 1 Months = Monthly
calcFrequency 4 Weeks  = EveryFourthWeek
calcFrequency 2 Weeks  = Biweekly
calcFrequency 1 Weeks  = Weekly
calcFrequency 1 Days   = Daily
calcFrequency _ _      = OtherFrequency

