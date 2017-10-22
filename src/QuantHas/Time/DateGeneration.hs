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

{-|
  Module: QuantHas.Time.DateGeneration
  Description: DateGeneration type - specifies how dates should be generated
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
-}

module QuantHas.Time.DateGeneration (module QuantHas.Time.DateGeneration) where

-- comments based on comments in time/dategenerationrule.hpp in Quantlib source

data DateGenerationRule
    = Backward         -- backward from termination date to effective date
      | Forward        -- forward from effective date to termination date
      | Zero           -- no dates between effective and termination dates
      | ThirdWednesday -- all but effective and term dates are 3ed Wednesdays of month
      | Twentieth      -- all but effective date are 20th of month (term date also modified)
      | TwentiethIMM   -- all but effective date are 20th of an IMM month (term date also modified
      | OldCDS         -- same as TwentiethIMM with unrestricted date ends and with log/stub coupon period
      | CDS            -- CDS standard rule since 2009
      deriving (Eq, Show)