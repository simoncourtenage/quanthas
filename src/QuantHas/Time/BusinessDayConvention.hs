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

module QuantHas.Time.BusinessDayConvention (module QuantHas.Time.BusinessDayConvention) where

{-|
  Module: QuantHas.Time.BusinessDayConvention
  Description: Specification of how dates that are not business days should be dealt with
  Copyright: (c) Simon Courtenage 2017
  Maintainer: courtenage@gmail.com
  Status: work in progress - will not compile
-}


-- |  See https://developers.opengamma.com/quantitative-research/Conventions-Single-Name-Credit-Default-Swaps-OpenGamma.pdf
-- A business day convention is a convention for dealing with a date when it is not a business day.  For example,
-- if a CDS falls due on a weekend, then one possibility is to take the next day following that is a business day
-- (this would be Following from the values below).
data BusinessDayConvention
    = Following
      | ModifiedFollowing
      | Preceding
      | ModifiedPreceding
      | Unadjusted
      | HalfMonthModifiedFollowing
      | Nearest
      deriving (Eq, Show)
      
