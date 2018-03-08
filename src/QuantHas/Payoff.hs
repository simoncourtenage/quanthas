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

module QuantHas.Payoff 
    (
        module QuantHas.Payoff
    )
    where

import QuantHas.Money

type PayoffName = String
type PayoffDescription = String

data Payoff = Payoff {
                  runPayoff :: Money -> Maybe Money
                , description :: PayoffDescription
                , name :: PayoffName
              }

mkPayoff :: (Money -> Maybe Money) -> PayoffName -> PayoffDescription -> Payoff
mkPayoff f n d = Payoff { runPayoff = f, name = n, description = d }

payoff :: Payoff -> Money -> Maybe Money
payoff p m = runPayoff p $ m




