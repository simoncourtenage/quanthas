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

module QuantHas.Exercise
    (
        module QuantHas.Exercise
    ) where

import QuantHas.Time.Date
import Data.Maybe (fromJust)

data ExerciseType = AmericanExercise | BermudanExercise | EuropeanExercise deriving (Eq,Show)

data Exercise
    = Exercise
        { 
            exerciseType :: ExerciseType
          , dates :: [Date]
          , payoffAtExpiry :: Maybe Bool
        }

lastExerciseDate :: Exercise -> Date
lastExerciseDate = last . dates

mkExercise :: ExerciseType -> [Date] -> Maybe Bool -> Exercise
mkExercise t ds mb = Exercise {exerciseType = t, dates = ds, payoffAtExpiry = mb }

mkAmericanExercise :: [Date] -> Bool -> Exercise
mkAmericanExercise ds b = mkExercise AmericanExercise ds' (Just b)
    where ds' = case length ds of
                1 -> minDate : [head ds]
                _ -> ds
          minDate = fromJust $ minimumDate

mkEuropeanExercise :: [Date] -> Exercise
mkEuropeanExercise ds = mkExercise EuropeanExercise ds Nothing

mkBermudanExercise :: [Date] -> Bool -> Exercise
mkBermudanExercise ds b = mkExercise BermudanExercise ds (Just b)