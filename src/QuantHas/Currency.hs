module QuantHas.Currency (module QuantHas.Currency) where

type Currency = String

currencySymbol :: Currency -> String
currencySymbol "GBP" = "£"
currencySymbol "USD" = "$"
currencySymbol "EUR" = "€"

