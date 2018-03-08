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

module QuantHas.Instruments.Option where

import QuantHas.Instruments.Instrument
import QuantHas.Quotes.Quote
import QuantHas.Quotes.SimpleQuote
import QuantHas.Money
import QuantHas.Payoff
import QuantHas.Instruments.Payoffs
import QuantHas.Exercise
import QuantHas.Instruments.OptionType

data Option
    = Option {
          optionType :: OptionType
        , optionPayoff :: Payoff
        , optionExercise :: Exercise
    }

mkOption :: OptionType -> Payoff -> Exercise -> Option
mkOption o p e = Option { optionType = o, optionPayoff = p, optionExercise = e }


