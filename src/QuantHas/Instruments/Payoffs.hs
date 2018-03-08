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

module QuantHas.Instruments.Payoffs 
    (
        module QuantHas.Instruments.Payoffs
    )
    where

import Data.Maybe (fromJust)
import QuantHas.Payoff
import QuantHas.Money
import QuantHas.Instruments.OptionType

mkNullPayoff :: Payoff
mkNullPayoff
    = mkPayoff nullPayoff "NullPayoff" "Null payoff"

nullPayoff :: Money -> Maybe Money
nullPayoff _ = Nothing

mkPlainVanillaPayoff :: OptionType -> Money -> Payoff
mkPlainVanillaPayoff o s = mkPayoff (pvPayoff o s) "Plain Vanilla Payoff" "Plain vanilla payoff"

pvPayoff :: OptionType -> Money -> Money -> Maybe Money
pvPayoff Call strike price
    = price `subtractMoney` strike >>= \x -> Just . Money (currency x) $ max (amount x) 0.0
pvPayoff Put strike price 
    = strike `subtractMoney` price >>= \x -> Just . Money (currency x) $ max (amount x) 0.0

percentPayoff :: OptionType -> Money -> Money -> Maybe Money
percentPayoff Call strike price
    = undefined