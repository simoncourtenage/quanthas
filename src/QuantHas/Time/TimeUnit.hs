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

-- | The 'TimeUnit' data type used for representing the common units of time.
--

module QuantHas.Time.TimeUnit (
    -- | The 'TimeUnit' data type with Days, Weeks, Months and Years as different units of time.
    TimeUnit (..)
) where

data TimeUnit
    = Days
    | Weeks
    | Months
    | Years
    deriving (Eq,Show)



