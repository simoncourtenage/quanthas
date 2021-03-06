{-
    Copyright (C) 2018, Simon Courtenage (courtenage@gmail.com)
    
    This file is part of QuantHas, an open-source Haskell implementation
    of the QuantLib library for quantitative finance.
    
    Quanthas is free software: you can redistribute it and/or modify it
    under the terms of the QuantHas license.  You should have received a
    copy of the license along with this program.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the license for more details.
-}

module QuantHas.Instruments.Instrument ( Instrument(..) ) where

import QuantHas.Time.Date
import QuantHas.Money

-- Instrument defines an interface that all Instrument instances need to implement
class Instrument a where
    npv :: a -> Money
    isExpired :: a -> Bool
    isExpired _ = False
    valuationDate :: a -> Maybe Date
    valuationDate _ = Nothing
    errorEstimate :: a -> Maybe Double
    errorEstimate _ = Nothing


    
	
     