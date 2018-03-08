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

module QuantHas.Money(module QuantHas.Money) where

import QuantHas.Currency

data Money = Money { currency::Currency,  amount::Double }
    deriving (Eq)

instance Show Money where
    show (Money c d) = currencySymbol c ++ showDouble d

instance Ord Money where
    compare (Money c d) (Money c' d')
        | c == c' = compare d d'
        | otherwise = error "Unable to compare money of different currencies"

mkMoney :: Currency -> Double -> Money
mkMoney c d = Money c d

showDouble :: Double -> String
showDouble d = show i ++ "." ++ sf
    where i = floor d
          f = 100 * (d - fromIntegral i)
          sf = show (round f)

addMoney :: Money -> Money -> Maybe Money
addMoney m m' | currency m == currency m' = Just $ Money (currency m) (amount m + amount m')
              | otherwise                 = Nothing

subtractMoney :: Money -> Money -> Maybe Money
subtractMoney m m' | currency m == currency m' = Just $ Money (currency m) (amount m - amount m')
                   | otherwise                 = Nothing
