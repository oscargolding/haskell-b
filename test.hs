-- learn.hs

module Learn where

import Data.Time.Format.ISO8601 (yearFormat)
import Distribution.Simple.Utils (xargs)

first = x * 3 + y
  where
    x = 3
    y = 1000

second = x * 5
  where
    x = 10 * 5 + y
    y = 10

third = z / x + y
  where
    x = 7
    y = negate x
    z = y * 10

waxOn = x * 5
  where
    x = y ^ 2
    y = z + 8
    z = 7

triple x = x * 3

waxOff x = triple x ^ 2