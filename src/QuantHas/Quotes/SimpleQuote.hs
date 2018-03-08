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

module QuantHas.Quotes.SimpleQuote
    (
          SimpleQuote
        , mkSimpleQuote
        , value
        , isValid
        , module QuantHas.Money
    )
    where

import QuantHas.Quotes.Quote
import QuantHas.Money

newtype SimpleQuote = SimpleQuote Money

instance Quote SimpleQuote where
    value (SimpleQuote d) = d
    isValid (SimpleQuote _) = True

mkSimpleQuote :: Money -> SimpleQuote
mkSimpleQuote d = SimpleQuote d 