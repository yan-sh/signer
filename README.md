Example of usage

Sometimes, we want to separate the responsibility for processing some data and checking that data has some properties. In example below, we have separeted module A.hs that has to pay salary, and module B.hs, that is responsible to prove that salary has certain amount. Function `paySafe` has constraint on its argument that doesn't allow to pay incorrect sum, and we have no choice but to use functions from B.hs to get "signed" salary

```haskell
===============================================================================

module A where

import Signer
import B
import Data.Fixed

paySafe :: HasSigns '[GreaterThan 0, LessThan 1000] x => Signed Centi x -> IO ()
paySafe signedSalary = ...

===============================================================================

module B (mkGreaterThan, mkLessThan, GreaterThan, LessThan) where

import Signer
import Data.Fixed

data GreaterThan x = GreaterThan
data LessThan x = LessThan

mkGreaterThan :: KnownNat n => Proxy n -> Signed Centi x -> Maybe (Signed Centi (GreaterThan n ': x))
mkGreaterThan p s =
  if unsign s > fromInteger (natVal p)
     then Just $ sign GreaterThan s
     else Nothing

mkLessThan :: KnownNat n => Proxy n -> Signed Centi x -> Maybe (Signed Centi (LessThan n ': x))
mkLessThan p s =
  if unsign s < fromInteger (natVal p)
     then Just $ sign LessThan s
     else Nothing

===============================================================================

module C where

import A (paySalary)
import B (mkGreaterThan, mkLessThan)

pay :: Centi -> IO ()
pay salary =
  case check (wrap salary) of
    Nothing -> pure ()
    Just signedSalary -> paySafe signedSalary
  where check = mkGreaterThan Proxy >=> mkLessThan Proxy

===============================================================================
```
