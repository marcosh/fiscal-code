module Data.FiscalCode.Internal.Omocodia where

-- base
import           Data.Char (isDigit)

convertDigit :: Char -> Char
convertDigit '0' = 'L'
convertDigit '1' = 'M'
convertDigit '2' = 'N'
convertDigit '3' = 'P'
convertDigit '4' = 'Q'
convertDigit '5' = 'R'
convertDigit '6' = 'S'
convertDigit '7' = 'T'
convertDigit '8' = 'U'
convertDigit '9' = 'V'
convertDigit c   = c

-- | starting from the right, replaces the first digit using the `convertDigit` function
omocodia :: String -> String
omocodia = reverse . fst . convert "" False . reverse
  where
    convert :: String -> Bool -> String -> (String, Bool)
    convert accumulator True  s      = (accumulator ++ s, True)
    convert accumulator False []     = (accumulator, False)
    convert accumulator False (x:xs) | isDigit x = (accumulator ++ convertDigit x : xs, True)
                                     | otherwise = convert (accumulator ++ [x]) False xs
