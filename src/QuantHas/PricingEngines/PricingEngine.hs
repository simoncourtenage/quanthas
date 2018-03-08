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

module QuantHas.PricingEngines.PricingEngine
    (
        module QuantHas.PricingEngines.PricingEngine
    ) where

import QuantHas.Money
import QuantHas.Instruments.Instrument

-- some draft ideas for pricing engines
newtype PricingEngine a = PricingEngine { runEngine :: a -> Money }

price :: Instrument a => PricingEngine a -> a -> Money
price p i = (runEngine p) i

idEngine :: Instrument a => PricingEngine a
idEngine = PricingEngine $ npv


