{-
    Copyright (C) 2010, Simon Courtenage (courtenage@gmail.com)
    
    This file is part of QuantHas, an open-source Haskell implementation
    of the QuantLib library for quantitative finance.
    
    QuantHas is free software: you can redistribute it and/or modify it
    under the terms of the QuantHas license.  You should have received a
    copy of the license along with this program.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the license for more details.
    
-}

module QuantHas.Util where

fromRight :: Either a b -> b
fromRight (Right b) = b

-- | split a list into (head, middle, last)
-- List must have a minimum length of 2
hml :: [a] -> Maybe (a,[a],a)
hml [] = Nothing
hml l | length l < 2 = Nothing
      | otherwise    = Just (head l, (init . tail) l , last l) 

