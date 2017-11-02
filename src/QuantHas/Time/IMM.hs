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

module QuantHas.Time.IMM (isIMMdate) where

{-|
  Module: QuantHas.Time.IMM
  Description: International Money Market dates - see https://en.wikipedia.org/wiki/IMM_dates
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
  Status: draft
-}

import QuantHas.Time.Date
import QuantHas.Time.DayName

{--
  IMM codes - see http://www.cmegroup.com/month-codes.html
  Also: https://en.wikipedia.org/wiki/Delivery_month

--}

-- | is a date an IMM date?  I.e., is it a third Wednesday in a month, and, if part of the main cycle of
-- quarterly expiry dates, is the month March, June, Sept or Dec?
isIMMdate :: 
       CalDate  -- ^ date to be checked
    -> Bool     -- ^ in QL code, called 'maincycle'
    -> Bool
isIMMdate d maincycle
    | getweekdayname d /= Wednesday   = False
    | getday d < 15 || getday d > 21  = False
    | not maincycle                   = True
    | elem (getmonth d) [3,6,9,12]    = True
    | otherwise                       = False
