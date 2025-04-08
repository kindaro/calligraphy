{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Calligraphy.Util.Assorti where

import GHC.Exts (IsString)
import Text.Printf

newtype NiceFloat = NiceFloat Float
instance Show NiceFloat where
  show (NiceFloat float) = printf "%.3f" float

newtype PrettyFraction α β = PrettyFraction (α, β)
instance (Show α, Show β) => Show (PrettyFraction α β) where
  show (PrettyFraction (x, y)) = show x <> " / " <> show y

newtype Percent = Percent Float
instance Show Percent where
  show (Percent float) =
    let percent = truncate (float * 100)
     in show @Int percent <> "." <> show @Int (round (float * 1000) `mod` 10) <> "%"

(<:>) :: (Show values) => String -> values -> String
name <:> value = name <> ": " <> show value
infix 6 <:>

parenthesize :: (IsString string, Semigroup string) => string -> string
parenthesize string = "(" <> string <> ")"

quote :: (IsString string, Semigroup string) => string -> string
quote string = "\"" <> string <> "\""

hexy :: Int -> String
hexy = printf "%02x"
